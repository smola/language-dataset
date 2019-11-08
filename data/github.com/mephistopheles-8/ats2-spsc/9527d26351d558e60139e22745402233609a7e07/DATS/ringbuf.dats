(** 
 ** Project : spsc
 ** Author  : Mark Bellaire
 ** Year    : 2019
 ** License : MIT
*)

#include "share/atspre_staload.hats"
#include "./../HATS/project.hats"

staload "./../SATS/INTERNAL/ringbuf.sats"
staload "./../SATS/INTERNAL/atomic.sats"

implement {a}
ringbuf_create{n}( sz ) =
  let
    var rb : ringbuf(a,n)
    val () = (
      rb.array := $UNSAFE.castvwtp0{ptr}( 
        arrayptr_make_uninitized<a>(sz)
      );
      rb.head := i2sz(0);
      rb.tail := i2sz(0);
      rb.size := sz;
      rb.onread := (lam() : void =<cloptr1> ());
      rb.onwrite := (lam () : void =<cloptr1> ());
    )
  in rb
  end


macdef cloptr_free( f ) =
 cloptr_free($UNSAFE.castvwtp0{cloptr(void)}(,(f))) 

implement (a:t0p)
ringbuf_free$clear<a>( x ) = () where { prval () = topize( x ) }

implement (a:vt0p)
ringbuf_free$clear<a>( x ) = gclear_ref<a>(x) 

implement {a}
ringbuf_free{n}( rb ) =
  let
    var rb : ringbuf(a,n) = rb
    (** Clear all events **)
    fun loop{n:pos}( rb: &ringbuf( a, n ) )
      : void =
      let
        var x : a?
      in if ringbuf_dequeue<a>(rb, x) 
         then
          let 
            prval () = opt_unsome( x )
            val () = ringbuf_free$clear( x )
           in loop( rb )
          end
         else 
          let
            prval () = opt_unnone(x) 
           in ()
          end
      end
    val () = loop( rb )
    val () 
      = cloptr_free( rb.onread )
    
    val () 
      = cloptr_free( rb.onwrite )
    
  in $extfcall( void, "atspre_mfree_gc", rb.array ) 
  end

// it's safe to read rb.head; the enqueueing thread
// is the only writer. It's not safe to *write*
// to rb.head, because it could be read by multiple 
// threads 

implement {a}
ringbuf_enqueue( rb, x ) =
  let
    val h = (rb.head + 1) mod rb.size
  in if h = atomic_read(rb.tail)  // the queue is full 
     then false where {
        prval () = opt_some( x )
      }
     else (
      atomic_write(rb.head,  h);
      rb.onwrite();
      true;
    ) where {
      
      val _ = 
        $UNSAFE.ptr0_set_at<a>( rb.array, rb.head, x );
      prval () = opt_none( x )
    } 
  end

// for t0p

implement {a}
ringbuf_enqueue0( rb, x ) =
  let
    val h = (rb.head + 1) mod rb.size
  in if h = atomic_read(rb.tail)  // the queue is full 
     then false 
     else (
      atomic_write(rb.head,  h);
      rb.onwrite();
      true;
    ) where {
      val _ = 
        $UNSAFE.ptr0_set_at<a>( rb.array, rb.head, x );
    } 
  end

// it's safe to read rb.tail; the dequeing thread
// is the only writer. It's not safe to *write*
// to rb.tail, because it could be read by multiple 
// threads 

implement {a}
ringbuf_dequeue( rb, x ) =
  if rb.tail = atomic_read(rb.head) // The queue is empty
  then 
    let
      prval () = opt_none( x )
     in false
    end 
  else 
    let
      val () = x :=  
        $UNSAFE.ptr0_get_at<a>( rb.array, rb.tail )
      val () = atomic_write(rb.tail,  (rb.tail + 1) mod rb.size )
      val ()  = rb.onread()
      prval () = opt_some( x )
    in true
    end

implement {}
ringbuf_onread( rb, pred )  =
  let
    val () = cloptr_free( rb.onread )
   in rb.onread :=  pred 
  end


implement {}
ringbuf_onwrite( rb, pred )  =
  let
    val () = cloptr_free( rb.onwrite )
   in rb.onwrite :=  pred 
  end




