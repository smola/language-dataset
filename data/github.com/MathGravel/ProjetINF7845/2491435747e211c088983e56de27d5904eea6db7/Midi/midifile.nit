module midifile

import midiheader
import miditrack


class MidiFile
	var filename : String
	var midiReader : FileReader is noinit
	var head : MidiHeader is noinit
	var body : Array[Track] = new Array[Track]

	init
	do
		midiReader= new FileReader.open(self.filename)
		assert self.canRead
		head = new MidiHeader(self.midiReader.read_bytes(14))
		var i = 0
		var startTime = 0
		var channel = -1
		var flag = 0x00u8
		var temp : nullable Byte
		print head
		while canRead do
			var trackTemp = new Track(i,self.midiReader.read_bytes(8))
			i+=1
			var buffer = new Bytes.empty
			var readed = 0
			print "Track {trackTemp}"
			loop
				var deltaT = -1
				buffer = new Bytes.empty
				buffer.add midiReader.read_byte.to_b
				while buffer[buffer.length-1] >= 0x80u8 do
					buffer.add midiReader.read_byte.to_b
				end
				deltaT = ("0x" + buffer.hexdigest).to_i
				readed+= buffer.length
				print "Le buffer {buffer.hexdigest}"
				buffer = new Bytes.empty
				startTime += deltaT
				if midiReader.peek(1)[0] >= 0x80u8 then 
					flag = midiReader.read_byte.to_b
					readed += 1
				end
				buffer.add flag
				assert buffer[0] > 0x7Fu8
				print buffer[0]
				if buffer[0] == 0xFFu8 then

					buffer.append midiReader.read_bytes(2) 
					while buffer[buffer.length-1] > 0x7Fu8 do 	buffer.add midiReader.read_byte.to_b
					var len = ("0x" + buffer.slice_from(2).hexdigest).to_i
					if len > 0 then buffer.append midiReader.read_bytes(len)

				else if buffer[0] == 0xF0u8 or buffer[0] == 0xF7u8 then
					buffer.add midiReader.read_byte.to_b
					while buffer[buffer.length-1] > 0x7Fu8 do 	buffer.add midiReader.read_byte.to_b
					var len = ("0x" + buffer.slice_from(1).hexdigest).to_i
					if len > 0 then buffer.append midiReader.read_bytes(len)

				else if buffer[0].is_between(0xC0u8,0xDFu8) then
					buffer.append midiReader.read_bytes(1)
				else
					buffer.append midiReader.read_bytes(2)
				end
				readed+= buffer.length -1
				print buffer.hexdigest

				trackTemp.add new MidiEvent(deltaT,startTime,buffer[0],buffer)
				if buffer[0] == 0xFFu8 and buffer[1] == 0x2Fu8 then break
				print "Size actual {readed}  {trackTemp.size}"
				assert readed <= trackTemp.size 
			end
			body.add trackTemp 

		end


		midiReader.close
		print "Le body {body.length}"
		linkNotes
		 i = 0
		var j = 0
		while i < body.length  do
			j = 0
			while j < body[i].notes.length -1 do

				print body[i].notes[j]
				j+=1
			end
			i+=1
		end

	end

	 fun linkNotes : Bool
	do
		var i = 0
		var j = 0
		while i < body.length  do
			j = 0
			while j < body[i].length -1 do
				if body[i][j].messag != null and body[i][j].messag.isNoteOnEvent then
					body[i].notes.add body[i][j].messag.as(ChannelVoiceMessage).clone
				else if body[i][j].messag != null and body[i][j].messag.isNoteOffEvent then
					body[i].notes[body[i].notes.length -1].setEndTime(body[i][j].startTime)
				end
				j+= 1
			end
			i+=1
		end
		return true
	end


	fun canRead: Bool do return not midiReader.eof
	redef fun to_s : String
	do
		return head.to_s
	end


end

if args.is_empty then
	print "Usage: ./midiparser file"
	exit(1)
end

var midi = new MidiFile(args[0])
