obj
	door
		proc
			receive(id in view(usr.client))
				if (id == "0")
					if (src.density)
						src.open()
					else
						src.close()
				else
					if (id == "1")
						if (src.density)
							src.open()
					else
						if (!( src.density ))
							src.close()


			open()
				if (src.operating)
					return
				src.operating = 1
				flick("door1_0", src)
				src.icon_state = "door0_0"
				sleep(6)
				src.density = 0
				src.opacity = 0
				sleep(2)
				src.operating = 0


			close()
				set src in oview(1)
				if ((src.operating || src.density))
					return
				src.operating = 1
				flick("door0_1", src)
				src.icon_state = "door1_1"
				sleep(2)
				src.density = 1
				src.opacity = 1
				sleep(6)
				src.operating = 0
		verb
			access(code as text)
				set src in oview(1)

				if (src.connected)
					src.connected.receive(code)

