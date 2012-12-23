package de.sciss.lucre
package expr

import de.sciss.lucre.{event => evt}

object ExtractorSuite extends App /* TODO: FunSuite */ {
   sealed trait Identified[ +S, +A ]
   case class IsVariable[ S <: evt.Sys[ S ], A ]( v: Expr.Var[ S, A ]) extends Identified[ S, A ]
   case class IsConstant[ A ]( value: A ) extends Identified[ Nothing, A ]
   case object IsOther extends Identified[ Nothing, Nothing ]

   def identify[ S <: evt.Sys[ S ], A ]( expr: Expr[ S, A ]) : Identified[ S, A ] = expr match {
      case Expr.Var( v )   => IsVariable( v )
      case Expr.Const( c ) => IsConstant( c )
      case _               => IsOther
   }

   type S = evt.InMemory
   implicit val sys = evt.InMemory()

   val strings = Strings[ S ]
   import strings._

   val _const  = Const( "hallo" )
   val _var    = sys.step { implicit tx => Var( "welt" )}
   val _other  = sys.step { implicit tx => _var.reverse }

   assert( identify( _const ) == IsConstant( "hallo" ))
   assert( identify( _var   ).isInstanceOf[ IsVariable[ _, _ ]])
   assert( identify( _other ) == IsOther )
   assert(  Expr.isConst( _const ))
   assert( !Expr.isConst( _var   ))
   assert( !Expr.isConst( _other ))
   println( "ExtractorSuite ok." )
}
