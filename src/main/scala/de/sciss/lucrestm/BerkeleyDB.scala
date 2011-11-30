/*
 *  BerkeleyDB.scala
 *  (LucreSTM)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucrestm

import de.sciss.lucrestm.{Ref => _Ref, Val => _Val}
import java.util.concurrent.ConcurrentLinkedQueue
import concurrent.stm.{TxnLocal, Txn, InTxnEnd, TxnExecutor, InTxn, Ref => ScalaRef}
import java.io.{FileNotFoundException, File, IOException}
import com.sleepycat.je.{DatabaseEntry, DatabaseConfig, EnvironmentConfig, TransactionConfig, Environment, Database, Transaction, OperationStatus}
import annotation.elidable
import elidable.CONFIG

object BerkeleyDB {
   import LucreSTM.logConfig

   /* private val */ var DB_CONSOLE_LOG_LEVEL   = "OFF" // "ALL"

   sealed trait ID extends Identifier[ InTxn ]

   def open( file: File, createIfNecessary: Boolean = true ) : BerkeleyDB = {
      val exists = file.isFile
      if( !exists && !createIfNecessary ) throw new FileNotFoundException( file.toString )

      val envCfg  = new EnvironmentConfig()
      val txnCfg  = new TransactionConfig()
      val dbCfg   = new DatabaseConfig()

      envCfg.setTransactional( true )
      envCfg.setAllowCreate( createIfNecessary )
      dbCfg.setTransactional( true )
      dbCfg.setAllowCreate( createIfNecessary )

      val dir     = file.getParentFile
      val name    = file.getName
      if( !exists ) dir.mkdirs()

//    envCfg.setConfigParam( EnvironmentConfig.FILE_LOGGING_LEVEL, "ALL" )
      envCfg.setConfigParam( EnvironmentConfig.CONSOLE_LOGGING_LEVEL, DB_CONSOLE_LOG_LEVEL )
      val env     = new Environment( dir, envCfg )
      val txn     = env.beginTransaction( null, txnCfg )
      try {
         txn.setName( "Open '" + name + "'" )
         val db      = env.openDatabase( txn, name, dbCfg )
         val ke      = new DatabaseEntry( Array[ Byte ]( 0, 0, 0, 0 ))  // key for last-key
         val ve      = new DatabaseEntry()
         val cnt     = if( db.get( txn, ke, ve, null ) == OperationStatus.SUCCESS ) {
            val in   = new DataInput( ve.getData, ve.getOffset, ve.getSize )
            in.readInt()
         } else 0
         txn.commit()
         new System( env, db, txnCfg, ScalaRef( cnt ))
      } catch {
         case e =>
            txn.abort()
            throw e
      }
   }

   private final class System( env: Environment, db: Database, txnCfg: TransactionConfig, idCnt: ScalaRef[ Int ])
   extends BerkeleyDB with Txn.ExternalDecider {
      system =>

//      private val peer        = new CCSTM()
//      private val idCnt       = ScalaRef( 0 ) // peer.newRef( 0 )
      private val dbTxnSTMRef = TxnLocal( initialValue = initDBTxn( _ ))
      private val ioQueue     = new ConcurrentLinkedQueue[ IO ]

      def root[ A ]( init: => A )( implicit tx: InTxn, ser: Serializer[ A ]) : A = {
         val rootID = 1
         tryRead[ A ]( rootID )( ser.read( _ )).getOrElse {
//println( "HERE CALLING NEWID" )
            val id   = newIDValue
//println( "DID CALL NEWID" )
            require( id == rootID, "Root can only be initialized on an empty database" )
//println( "CALLING INIT" )
            val res  = init
            write( id )( ser.write( res, _ ))
            res
         }
      }

      def atomic[ Z ]( block: InTxn => Z ) : Z = TxnExecutor.defaultAtomic( block )

      def debugListUserRecords()( implicit tx: InTxn ) : Seq[ ID ] = {
         val b   = Seq.newBuilder[ ID ]
         val cnt = idCnt.get
         var i = 1; while( i < cnt ) {
            if( tryRead[ Unit ]( i )( _ => () ).isDefined ) b += new IDImpl( i )
         i += 1 }
         b.result()
      }

      def newVal[ A ]( init: A )( implicit tx: InTxn, ser: Serializer[ A ]) : Val[ A ] = {
         val res = new ValImpl[ A ]( newIDValue, ser )
         res.setInit( init )
         res
      }

      def newInt( init: Int )( implicit tx: InTxn ) : Val[ Int ] = {
         val res = new IntVal( newIDValue )
         res.setInit( init )
         res
      }

//      def newRef[ A <: Disposable[ InTxn ]]()( implicit tx: InTxn, ser: Serializer[ A ]) : Ref[ A ] =
//         newRef[ A ]( EmptyMut )

      def newRef[ A <: Mutable[ BerkeleyDB ]]( init: A )( implicit tx: InTxn,
                                                          reader: MutableReader[ BerkeleyDB, A ]) : Ref[ A ] = {
         val res = new RefImpl[ A ]( newIDValue, reader )
         res.setInit( init )
         res
      }

      def newOptionRef[ A <: MutableOption[ BerkeleyDB ]]( init: A )( implicit tx: InTxn,
                                                           reader: MutableOptionReader[ BerkeleyDB, A ]) : Ref[ A ] = {
         val res = new OptionRefImpl[ A ]( newIDValue, reader )
         res.setInit( init )
         res
      }

//      def newMut[ A <: Disposable[ InTxn ]]( init: A )( implicit tx: InTxn, ser: Serializer[ A ]) : Mut[ A ] = {
//         val id   = newID
//         val res  = new MutImpl[ A ]( id, ser )
//         write( id )( ser.write( init, _ ))
//         res
//      }

      def newValArray[ A ]( size: Int ) : Array[ Val[ A ]] = new Array[ Val[ A ]]( size )

      def newRefArray[ A /*<: Mutable[ BerkeleyDB ]*/]( size: Int ) : Array[ Ref[ A ]] = new Array[ Ref[ A ]]( size )

      def readVal[ A ]( in: DataInput )( implicit ser: Serializer[ A ]) : Val[ A ] = {
         val id = in.readInt()
         new ValImpl[ A ]( id, ser )
      }

      def readInt( in: DataInput ) : Val[ Int ] = {
         val id = in.readInt()
         new IntVal( id )
      }

      def readRef[ A <: Mutable[ BerkeleyDB ]]( in: DataInput )
                                              ( implicit reader: MutableReader[ BerkeleyDB, A ]) : Ref[ A ] = {
         val id = in.readInt()
         new RefImpl[ A ]( id, reader )
      }

      def readOptionRef[ A <: MutableOption[ BerkeleyDB ]]( in: DataInput )
                                                          ( implicit reader: MutableOptionReader[ BerkeleyDB, A ]) : Ref[ A ] = {
         val id = in.readInt()
         new OptionRefImpl[ A ]( id, reader )
      }

//      def readMut[ A <: Mutable[ BerkeleyDB ]]( in: DataInput )( constr: ID => A ) : A = {
//         val id = new IDImpl( in.readInt() )
//         constr( id )
//      }

      def readMut[ A <: Mutable[ BerkeleyDB ]]( in: DataInput )( implicit reader: MutableReader[ BerkeleyDB, A ]) : A = {
         val id = new IDImpl( in.readInt() )
         reader.readData( in, id )
      }

      def readOptionMut[ A <: MutableOption[ BerkeleyDB ]]( in: DataInput )
                                                          ( implicit reader: MutableOptionReader[ BerkeleyDB, A ]) : A = {
         val mid = in.readInt()
         if( mid == -1 ) reader.empty else {
            reader.readData( in, new IDImpl( mid ))
         }
      }

      def close() { db.close() }

      def numRecords : Long = db.count()
      def numUserRecords : Long = math.max( 0L, db.count() - 1 )

      private def txnHandle( implicit txn: InTxnEnd ) : Transaction = dbTxnSTMRef.get

      private def initDBTxn( implicit txn: InTxn ) : Transaction = {
         Txn.setExternalDecider( this )
         val dbTxn = env.beginTransaction( null, txnCfg )
         logConfig( "txn begin " + dbTxn.getId )
         Txn.afterRollback { status =>
            try {
               logConfig( "txn rollback " + dbTxn.getId )
               dbTxn.abort()
            } catch {
               case _ =>
            }
         }
         dbTxn
      }

      private def newIDValue( implicit tx: InTxn ) : Int = {
//      val id = idCnt.transformAndGet( _ + 1 )
         val id = idCnt.get + 1
         logConfig( "new " + id )
         idCnt.set( id )
         withIO { io =>
            val out = io.beginWrite()
            out.writeInt( id )
            io.endWrite( 0 )
         }
         id
      }

      def newID( implicit tx: InTxn ) : ID = new IDImpl( newIDValue )

      private def withIO[ A ]( fun: IO => A ) : A = {
         val ioOld   = ioQueue.poll()
         val io      = if( ioOld != null ) ioOld else new IO
         try {
            fun( io )
         } finally {
            ioQueue.offer( io )
         }
      }

      def write( id: Int )( valueFun: DataOutput => Unit )( implicit tx: InTxn ) {
         logConfig( "write " + id )
         withIO { io =>
            val out = io.beginWrite()
            valueFun( out )
            io.endWrite( id )
         }
      }

      def remove( id: Int )( implicit tx: InTxn ) {
         logConfig( "remove " + id )
         withIO( _.remove( id ))
      }

      def read[ @specialized A ]( id: Int )( valueFun: DataInput => A )( implicit tx: InTxn ) : A = {
         logConfig( "read " + id )
         withIO { io =>
            val in = io.read( id )
            if( in != null ) {
               valueFun( in )
            } else {
//            Txn.retry
               throw new IOException()
            }
         }
      }

      def tryRead[ A ]( id: Int )( valueFun: DataInput => A )( implicit tx: InTxn ) : Option[ A ] = {
//         logConfig( "try-read " + id )
         withIO { io =>
            val in = io.read( id )
            if( in != null ) Some( valueFun( in )) else None
         }
      }

      private sealed trait BasicSource {
         protected def id: Int

         final def write( out: DataOutput ) {
            out.writeInt( id )
         }

         final def dispose()( implicit tx: InTxn ) {
            system.remove( id )
         }

         @elidable(CONFIG) protected final def assertExists()( implicit tx: InTxn ) {
            require( system.tryRead[ Unit ]( id )( _ => () ).isDefined, "trying to write disposed ref " + id )
         }
      }

      private final class IDImpl( val id: Int ) extends ID {
         def write( out: DataOutput ) { out.writeInt( id )}

         override def equals( that: Any ) : Boolean = {
            /* (that != null) && */ that.isInstanceOf[ IDImpl ] && (id == that.asInstanceOf[ IDImpl ].id)
         }

         def dispose()( implicit tx: InTxn ) {
            system.remove( id )
         }

         override def toString = "<" + id + ">"
      }

      private final class ValImpl[ A ]( protected val id: Int, ser: Serializer[ A ])
      extends Val[ A ] with BasicSource {
         def get( implicit tx: InTxn ) : A = {
            system.read[ A ]( id )( ser.read( _ ))
         }

         def setInit( v: A )( implicit tx: InTxn ) {
            system.write( id )( ser.write( v, _ ))
         }

         def set( v: A )( implicit tx: InTxn ) {
            assertExists()
            system.write( id )( ser.write( v, _ ))
         }

         def transform( f: A => A )( implicit tx: InTxn ) { set( f( get ))}

         def debug() {
            println( "Val(" + id + ")" )
         }
      }

      private final class IntVal( protected val id: Int ) extends Val[ Int ] with BasicSource {
         def get( implicit tx: InTxn ) : Int = {
            system.read[ Int ]( id )( _.readInt() )
         }

         def setInit( v: Int )( implicit tx: InTxn ) {
            system.write( id )( _.writeInt( v ))
         }

         def set( v: Int )( implicit tx: InTxn ) {
            assertExists()
            system.write( id )( _.writeInt( v ))
         }

         def transform( f: Int => Int )( implicit tx: InTxn ) { set( f( get ))}

         def debug() {
            println( "Val[Int](" + id + ")" )
         }
      }


      private final class RefImpl[ A <: Mutable[ BerkeleyDB ]]( protected val id: Int,
                                                                val reader: MutableReader[ BerkeleyDB, A ])
      extends Ref[ A ] with BasicSource {
         def debug() {
            println( "Ref(" + id + ")" )
         }

         def get( implicit tx: InTxn ) : A = {
            system.read[ A ]( id ) { in =>
               val mid = in.readInt()
               reader.readData( in, new IDImpl( mid ))
            }
         }

         def setInit( v: A )( implicit tx: InTxn ) {
            system.write( id )( v.write( _ ))
         }

         def set( v: A )( implicit tx: InTxn ) {
            assertExists()
            system.write( id )( v.write( _ ))
         }

         def transform( f: A => A )( implicit tx: InTxn ) { set( f( get ))}
      }

      private final class OptionRefImpl[ A <: MutableOption[ BerkeleyDB ]](
         protected val id: Int, val reader: MutableOptionReader[ BerkeleyDB, A ])
      extends Ref[ A ] with BasicSource {
         def debug() {
            println( "Ref(" + id + ")" )
         }

         def get( implicit tx: InTxn ) : A = {
            system.read[ A ]( id ) { in =>
               val mid = in.readInt()
               if( mid == -1 ) reader.empty else {
                  reader.readData( in, new IDImpl( mid ))
               }
            }
         }

         def setInit( v: A )( implicit tx: InTxn ) {
            system.write( id ) { out =>
               v match {
                  case m: Mutable[ _ ] => m.write( out )
                  case _: EmptyMutable => out.writeInt( -1 )
               }
            }
         }

         def set( v: A )( implicit tx: InTxn ) {
            assertExists()
            system.write( id ) { out =>
               v match {
                  case m: Mutable[ _ ] => m.write( out )
                  case _: EmptyMutable => out.writeInt( -1 )
               }
            }
         }

         def transform( f: A => A )( implicit tx: InTxn ) { set( f( get ))}
      }

      private final class IO {
         private val keyArr   = new Array[ Byte ]( 4 )
         private val keyE     = new DatabaseEntry( keyArr )
         private val valueE   = new DatabaseEntry()
         private val out      = new DataOutput()

         def beginWrite() : DataOutput = {
            out.reset()
            out
         }

         private def keyToArray( key: Int ) {
            val a    = keyArr
            a( 0 )   = (key >> 24).toByte
            a( 1 )   = (key >> 16).toByte
            a( 2 )   = (key >>  8).toByte
            a( 3 )   = key.toByte
         }

         def read( key: Int )( implicit tx: InTxn ) : DataInput = {
            val h    = txnHandle
            keyToArray( key )
            val ve   = valueE
            if( db.get( h, keyE, ve, null ) == OperationStatus.SUCCESS ) {
               new DataInput( ve.getData, ve.getOffset, ve.getSize )
            } else {
               null
            }
         }

         def remove( key: Int )( implicit tx: InTxn ) {
            val h    = txnHandle
            keyToArray( key )
            db.delete( h, keyE )
         }

         def endWrite( key: Int )( implicit tx: InTxn ) {
            val h    = txnHandle
            keyToArray( key )
            out.flush()
            valueE.setData( out.toByteArray )
            db.put( h, keyE, valueE )
         }
      }

      def shouldCommit( implicit txn: InTxnEnd ) : Boolean = {
         val h = dbTxnSTMRef.get
         try {
            logConfig( "txn commit " + h.getId )
            h.commit()
            true
         } catch {
            case e =>
               try {
                  logConfig( "txn abort " + h.getId )
                  h.abort()
               } catch {
                  case _ =>
               }
               false
         }
      }
   }

   sealed trait Ref[ A ] extends _Ref[ InTxn, /* Mut, */ A ] {
//      private[BerkeleyDB] def id: Int
//      protected def id: Int
//      def debug() {
//         println( "ID = " + id )
//      }
   }

//   sealed trait Mut[ +A ] extends Mutable[ InTxn, A ]
   sealed trait Val[ @specialized A ] extends _Val[ InTxn, A ]
}
sealed trait BerkeleyDB extends Sys[ BerkeleyDB ] {
   type Val[ @specialized A ]  = BerkeleyDB.Val[ A ]
   type Ref[ A ]  = BerkeleyDB.Ref[ A ]
//   type Mut[ +A ] = BerkeleyDB.Mut[ A ]
   type ID        = BerkeleyDB.ID
   type Tx        = InTxn

   /**
    * Closes the underlying database. The STM cannot be used beyond this call.
    */
   def close() : Unit

   /**
    * Reports the current number of records stored in the database.
    */
   def numRecords: Long

   /**
    * Reports the current number of user records stored in the database.
    * That is the number of records minus those records used for
    * database maintenance.
    */
   def numUserRecords : Long

   def debugListUserRecords()( implicit tx: InTxn) : Seq[ ID ]

   /**
    * Reads the root object representing the stored datastructure,
    * or provides a newly initialized one via the `init` argument,
    * if no root has been stored yet.
    */
   def root[ A ]( init: => A )( implicit tx: Tx, ser: Serializer[ A ]) : A
}