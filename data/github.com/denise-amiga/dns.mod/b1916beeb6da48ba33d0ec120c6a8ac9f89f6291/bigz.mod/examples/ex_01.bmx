SuperStrict

Framework brl.standardio
Import dns.bigz

'Print BzVersion()

Local a:TBZ = New TBZ(2)
Local b:TBZ = New TBZ(5)
Local c:TBZ = New TBZ(0)

Rem

a.bz=BzPow(a.bz, 9)
BzDebug("",a.bz)

a.bz=BzAsh(a.bz, 5)
BzDebug("",a.bz)

a.bz=BzAsh(a.bz, 5)
BzDebug("",a.bz)

a.bz=BzAsh(a.bz, -8)
BzDebug("",a.bz)

a.bz=BzAsh(a.bz, 72)
BzDebug("",a.bz)

a.bz=BzAsh(a.bz, -90)
BzDebug("",a.bz)

a.set(12345)
b.set(54321)
c = a & b
BzDebug(" and ",c.bz)
c = a | b
BzDebug(" or ",c.bz)
c = a ~ b
BzDebug(" xor ",c.bz)


a.set(1)
c = a shl 9
BzDebug("a shl 9",c.bz)
c = c shr 9
BzDebug("a shr 9",c.bz)

a :shl 9 
BzDebug(":shl 9",a.bz)
a :shr 9 
BzDebug(":shr 9",a.bz)

EndRem

a.set(1)
BzDebug("1",a.bz)
a = a + 1
BzDebug("2",a.bz)
a = a | 5
BzDebug("7",a.bz)
a :| 10
BzDebug("15",a.bz)
a :~ 255
BzDebug("15",a.bz)

c.Set(1)
For Local i:Int = 1 To 100
	b.Set(i)
	c:*b
Next
BzDebug("100!",c.bz)

c.Set(1)
For Local i:Int = 1 To 100
	c:*i
Next
BzDebug("100!",c.bz)

a.set(100)
c.bz=BzFactorial(a.bz)
BzDebug("100!"),c.bz
