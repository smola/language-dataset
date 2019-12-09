
shh dogescript powder.djs > powder.js

very ctx
very XRES is 576
very YRES is 384
very NPARTS is XRES * YRES

very pmap is []
very parts is []
very empty is []
very types is {}

very mouse is {}
mouse.x is 0
mouse.y is 0
mouse.down is false

very SOLID is 0
very LIQUID is 1
very GAS is 2
very POWDER is 3

such Type much name color state mass
    this.name is name || ""
    this.color is color || "#000"

    this.state is state || SOLID
    this.mass is mass || 1

    this.init is null
    this.update is null
    this.flammable is false
wow

such Particle much id x y type
    this.id is id
    this.x is x
    this.y is y
    this.type is type
    this.life is 0
    this.ctype is null
wow

very pensize is 10
very pentype is types.NONE

very lastframe
very fps

types.NONE is new Type with "NONE" "#FFF" SOLID 0
types.DMND is new Type with "DMND" "#CCFFFF" SOLID 5
types.DUST is new Type with "DUST" "#FFE0A0" POWDER 2
types.GAS is new Type with "GAS" "#E0FF20" GAS 0
types.GAS.flammable is true
types.WATR is new Type with "WATR" "#2030FF" LIQUID 1.5
types.WTRV is new Type with "WTRV" "#A0A0FF" GAS 0
types.FIRE is new Type with "FIRE" "#FF0000" GAS -2
types.CLNE is new Type with "CLNE" "#FFFF00" SOLID 4

such wtrv_init much p
    p.life is Math.random()*200+50
wow
such wtrv_update much p
    p.life is p.life - 1
    rly p.life smallerish 0
        p.type is types.WATR
    wow
wow true
types.WTRV.update is wtrv_update

such fire_init much p
    p.life is Math.random()*100+50
wow
such fire_update much p
    p.life is p.life - 1
    rly p.life smallerish 0
        plz despawn with p
        return false
    wow
    such c much p
        rly p.type is types.WATR
            p.type is types.WTRV
            p.life is Math.random()*200+50
        but rly p.type.flammable
            p.type is types.FIRE
        wow
    wow
    plz neighbours with p c 2
wow true
types.FIRE.init is fire_init
types.FIRE.update is fire_update

such clne_update much p
    very dx is -1
    very dy is -1
    much dy; dy smallerish 1; dy more 1
        much dx; dx smallerish 1; dx more 1
            very pn is pmap[p.y+dy][p.x+dx]
            rly pn
                rly !p.ctype and pn.type not types.CLNE
                    p.ctype is pn.type
                wow
            but
                rly p.ctype
                    plz spawn with p.ctype p.x+dx p.y+dy
                wow
            wow
        wow
    wow
wow
types.CLNE.update is clne_update

such neighbours much p c r
    very dx is -r
    very dy is -r
    much dy; dy smallerish r; dy more 1
        much dx; dx smallerish r; dx more 1
            very pn is pmap[p.y+dy][p.x+dx]
            rly pn
                plz c with pn
            wow
        wow
    wow
wow

such displace much p nx ny
    rly nx smaller 0 or ny smaller 0 or nx biggerish XRES or ny biggerish YRES
        return false
    wow

    rly p.x is nx and p.y is ny
        return true
    wow

    very r is pmap[ny][nx]
    rly r
        rly r.type.mass smallerish p.type.mass
            pmap[r.y][r.x] is p
            pmap[p.y][p.x] is r

            r.x is p.x
            r.y is p.y
            p.x is nx
            p.y is ny
            return true
        wow
    but
        pmap[p.y][p.x] is null
        pmap[ny][nx] is p

        p.x is nx
        p.y is ny
    wow
    return true
wow

such update much p
    rly p.type.update
        very r is plz p.type.update with p
        rly !r
            return
        wow
    wow

    very dx is 0
    very dy is 0
    rly p.type.state is GAS
        dx is Math.floor(Math.random()*3-1)
        dy is Math.floor(Math.random()*p.type.mass*2-p.type.mass)
    but rly p.type.state is LIQUID
        dx is Math.floor(Math.random()*3-1)
        dy is Math.floor(Math.random()*p.type.mass*2)
    but rly p.type.state is POWDER
        dx is Math.round(Math.random()*2-1)
        dy is Math.floor(Math.random()*p.type.mass*2)
    but rly p.type.state is SOLID
        return
    wow

    very nx is p.x+dx
    very ny is p.y+dy

    very r is plz displace with p nx ny
    rly r is false
        plz despawn with p
    wow
wow

such spawn much type x y
    rly !pmap[y][x]
        very id is plz empty.pop
        rly id is undefined
          id is parts.length
        wow
        very p is new Particle with id x y type
        rly p.type.init
            plz p.type.init with p
        wow
        parts[id] is p
        pmap[y][x] is p
    wow
wow

such despawn much p
    pmap[p.y][p.x] is null
    parts[p.id] is null
    plz empty.push with p.id
wow

such init
    very canvas is plz $ with "#screen"
    canvas[0].width is XRES
    canvas[0].height is YRES

    plz canvas.on with "mousemove" much e
        mouse.x is e.offsetX
        mouse.y is e.offsetY
    wow&
    plz canvas.on with "mousedown" much e
        mouse.down is true
    wow&
    plz canvas.on with "mouseup" much e
        mouse.down is false
    wow&
    plz canvas.on with "mousewheel" much e
        rly e.originalEvent.wheelDelta bigger 0
            pensize is pensize + 1
        but rly pensize bigger 1
            pensize is pensize - 1
        wow
    wow&

    much y=0 next y smaller YRES next y more 1
        pmap[y] is []
        much x=0 next x smaller XRES next x more 1
            pmap[y][x] is null
        wow
    wow

    ctx is canvas[0] dose getContext with "2d"

    plz requestAnimationFrame with frame
wow

such frame
    plz requestAnimationFrame with frame

    rly !lastframe
      lastframe is plz Date.now
      fps is 0
    but
      very delta is (new Date().getTime() - lastframe) / 1000
      lastframe is plz Date.now
      fps is 1 / delta
    wow

    ctx dose clearRect with 0 0 XRES YRES

    very pcount is 0

    much i=0 next i smaller parts.length next i more 1
        very p is parts[i]
        rly p
            pcount is pcount + 1
            plz update with p
            ctx.fillStyle is p.type.color
            ctx dose fillRect with p.x p.y 1 1
        wow
    wow

    ctx.strokeStyle is "#fff"
    ctx.fillStyle is "#fff"
    ctx dose strokeRect with mouse.x-pensize/2 mouse.y-pensize/2 pensize pensize

    very pcount is pcount.toString() + " parts"
    very palloc is parts.length.toString() + " alloc"
    very pfps is Math.floor(fps).toString() + " fps"
    ctx dose fillText with pcount 5 10
    ctx dose fillText with palloc 5 20
    ctx dose fillText with pfps 5 30

    very x is 5
    much i in types
        ctx.fillStyle is types[i].color
        ctx dose fillRect with x YRES-20 45 15
        ctx.fillStyle is "#000"
        ctx dose fillText with types[i].name x+8 YRES-9

        rly mouse.down and mouse.x bigger x and mouse.y bigger YRES-20 and mouse.x smallerish x+45 and mouse.y smallerish YRES-5
            pentype is types[i]
            mouse.down is false
        wow

        rly pentype is types[i]
            ctx.strokeStyle is "#F00"
            ctx dose strokeRect with x YRES-20 45 15
        wow

        x is x + 50
    wow

    rly mouse.down
        much dy=-Math.ceil(pensize/2) next dy smaller Math.floor(pensize/2) next dy more 1
            much dx=-Math.ceil(pensize/2) next dx smaller Math.floor(pensize/2) next dx more 1
                very x is mouse.x+dx
                very y is mouse.y+dy
                very p is pmap[y][x]

                rly pentype is types.NONE
                    rly p
                        plz despawn with p
                    wow
                but rly p
                    p.ctype is pentype
                but
                    plz spawn with pentype x y
                wow
            wow
        wow
    wow
wow

plz $ with init
