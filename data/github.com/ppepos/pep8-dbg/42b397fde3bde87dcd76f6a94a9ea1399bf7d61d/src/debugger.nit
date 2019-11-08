import config_
import interpreter
import disasm
import readline_

redef class Deserializer
	redef fun deserialize_class(name)
	do
		if name == "Array[DebuggerCommand]" then return new Array[DebuggerCommand].from_deserializer(self)
		return super
	end
end

class DebuggerController
	var interpreter: DebuggerInterpreter
	var disasm: Disassembler
	var src_nb_instr: Int = config_manager.debugger_config.context_nb_instructions
	var nb_bytes_disass: Int = config_manager.debugger_config.nb_bytes_to_disass

	init with_model(model: Pep8Model) do
		var reg_file = new Pep8RegisterFile
		disasm = new Disassembler(model)

		interpreter = new DebuggerInterpreter(model, reg_file)

		init(interpreter, disasm)
	end

	fun cont do interpreter.cont
	fun reverse_cont do interpreter.reverse_cont
	fun nexti do interpreter.nexti
	fun reverse_nexti do interpreter.reverse_nexti
	fun stepo do interpreter.stepo
	fun set_breakpoint(addr: Int) do interpreter.set_breakpoint addr
	fun remove_breakpoint(addr: Int) do interpreter.remove_breakpoint addr
	fun breakpoints: Array[Int] do return interpreter.breakpoints
	fun memory(addr, length: Int): Array[Byte] do return interpreter.memory_chunk(addr, length)
	fun reg_file: Pep8RegisterFile do return interpreter.reg_file
	fun disassemble(addr, length: Int): String do
		var mem = memory(addr, length)
		return self.disasm.disassemble_stream(mem, length, true, reg_file.pc.value)
	end
	fun run do self.interpreter.start
	fun source: String do
		var nb_lines = src_nb_instr
		var current_pc = reg_file.pc.value

		var src_instr = interpreter.source_instr(current_pc)
		var src_line = interpreter.source_line(current_pc)

		var current_instr = disasm.decode_next_instruction(memory(current_pc, 3))

		# Current memory differs from the source, so we use the disassembler instead
		if src_instr == null or src_instr != current_instr then
			return disassemble(reg_file.pc.value, nb_bytes_disass)
		end

		var out = ""
		# Get the next lines from the source file
		for i in [0..nb_lines[ do
			var out_template = "{current_pc.to_hex.justify(4, 1.0, '0')} "
			if src_line == null then return out
			out += out_template + src_line + "\n"
			current_pc += src_instr.len
			src_line = interpreter.source_line(current_pc)
			src_instr = interpreter.source_instr(current_pc)
		end

		return out
	end

end

class DebuggerCommand
	serialize
	var command_str: String
	var is_execution_cmd: Bool
	var alternative_names = new Array[String]
	var nb_tokens: Int
	var command_help_str: String
	var help_string: String

	fun help_str: String do return command_help_str.justify(34, 0.0) + ":   " + help_string.justify(37, 0.0)
	fun usage_str: String do return "Usage: {command_help_str}"
	fun is_command(command: String): Bool do return command_str == command or alternative_names.has(command)
end

class DebuggerCLI
	var ctrl: DebuggerController
	var rl = new Readline.with_mode(config_manager.debugger_config.cli_mode)
	var commands_def: Array[DebuggerCommand] is noinit
	var last_command: nullable DebuggerCommand = null
	var context_panels: Array[String] = config_manager.debugger_config.context_panels

	init with_model(model: Pep8Model) do
		var ctrl = new DebuggerController.with_model(model)
		init(ctrl)
	end

	init with_command_file(model: Pep8Model, file_path: String) do
		load_commands(file_path)
		with_model(model)
	end

	fun preprocess_command(input: String): String do
		var out = new Array[String]
		for sub in input.split(" ") do
			if sub.has_prefix("$") then sub = preprocess_replace_register(sub)
			out.add sub
		end
		return out.join(" ")
	end

	fun preprocess_replace_register(reg_name: String): String do
		reg_name = reg_name.substring_from(1)
		reg_name = reg_name.to_lower
		if reg_name == "a" then return ctrl.reg_file.a.value.to_s
		if reg_name == "x" then return ctrl.reg_file.x.value.to_s
		if reg_name == "pc" then return ctrl.reg_file.pc.value.to_s
		if reg_name == "sp" then return ctrl.reg_file.sp.value.to_s
		if reg_name == "n" then return ctrl.reg_file.n.value.to_s
		if reg_name == "z" then return ctrl.reg_file.z.value.to_s
		if reg_name == "v" then return ctrl.reg_file.v.value.to_s
		if reg_name == "c" then return ctrl.reg_file.c.value.to_s
		return "Cannot recognize register" + reg_name
	end

	fun parse_command(input: String) do
		var parsed_input = preprocess_command(input)

		var tokens = parsed_input.split(" ")

		if tokens.is_empty then return

		var cmd = tokens[0]
		var cmd_def = null

		for command_def in commands_def do
			if command_def.is_command(cmd) then
				cmd_def = command_def
				break
			end
		end

		if cmd_def == null then
			print "Unknown command: {cmd}"
			return
		end

		var cmd_str = cmd_def.command_str

		last_command = cmd_def

		if tokens.length != cmd_def.nb_tokens then
			print cmd_def.usage_str
		else if cmd_str == "break" then
			if tokens[1].is_int then
				ctrl.set_breakpoint tokens[1].to_i
			else
				print cmd_def.usage_str
			end
		else if cmd_str == "remove" then
			if tokens[1].is_int then
				ctrl.remove_breakpoint tokens[1].to_i
			else
				print cmd_def.usage_str
			end
		else if cmd_str == "nexti" then
			ctrl.nexti
		else if cmd_str == "rev-nexti" then
			ctrl.reverse_nexti
		else if cmd_str == "continue" then
			ctrl.cont
		else if cmd_str == "rev-continue" then
			ctrl.reverse_cont
		else if cmd_str == "stepo" then
			ctrl.stepo
		else if cmd_str == "regs" then
			print_reg
		else if cmd_str == "dump" then
			if tokens[1].is_int and tokens[2].is_int then
				dump_mem(tokens[1].to_i, tokens[2].to_i)
			else
				print cmd_def.usage_str
			end
		else if cmd_str == "disass" then
			if tokens[1].is_int and tokens[2].is_int then
				disass(tokens[1].to_i, tokens[2].to_i)
			else
				print cmd_def.usage_str
			end
		else if cmd_str == "run" then
			ctrl.run
		else if cmd_str == "help" then
			print_help
		else if cmd_str == "list" then
			print "Breakpoints : "
			print ctrl.breakpoints
		else if cmd_str == "quit" then
			exit(0)
		end
	end

	fun print_reg do
		var regs = ctrl.reg_file
		print "=========================="
		print "A : {regs.a.value}"
		print "X : {regs.x.value}"
		print "PC : {regs.pc.value}"
		print "SP : {regs.sp.value}"
		print "N : {regs.n.value} Z: {regs.z.value} V: {regs.v.value} C: {regs.c.value}"
		print "=========================="
	end

	fun print_help do
		var line = "=" * 80
		print line
		print "COMMAND LIST".justify(80, 0.5)
		print line
		for command in commands_def do
			print command.help_str
		end
		print line
	end

	fun dump_mem(addr, len: Int) do
		var mem = ctrl.memory(addr, len)

		for byte in mem, i in [0..len[ do
			if i % 16 == 0 then print ""
			printn "{byte} "
		end
		print ""
	end

	fun disass(addr, len: Int) do
		print ctrl.disassemble(addr, len)
	end

	fun load_commands(file_path: String) do
		var fd = new FileReader.open(file_path)
		var commands_json = fd.read_all

		var deserializer = new JsonDeserializer(commands_json)
		var commands = deserializer.deserialize

		assert commands isa Array[DebuggerCommand]
		var cmd = commands[0]

		commands_def = commands
	end

	fun print_context do
		for panel_pos in [0..context_panels.length[ do print_panel(panel_pos)
		print ""
	end

	fun print_panel(panel_pos: Int) do
		if context_panels.has("r") and context_panels[panel_pos] == "r" then
			print_reg
		else if context_panels.has("c") and context_panels[panel_pos] == "c" then
			print ctrl.source
		end
	end



	fun command_loop do
		var input
		var with_history = true
		var last_cmd

		loop
			last_cmd = last_command
			if last_cmd != null and last_cmd.is_execution_cmd then print_context

			input = rl.readline("PEPdb> ", with_history)

			# EOF
			if input == null then return

			# Sending an empty line replays the last command
			if input == "" then
				if last_command != null then parse_command last_command.command_str
			else
				parse_command input
			end
		end
	end
end

if args.length != 1 then
	print "Usage: {program_name} <source_file.pep>"
	exit(1)
end

var source_file = args[0]
var model = new Pep8Model(source_file)
model.load_instruction_set("src/pep8.json")
model.read_instructions

var debugger = new DebuggerCLI.with_command_file(model, "src/commands.json")
debugger.command_loop
