import opts

abstract class Command
	var name: String
	var description: String
	var options = new OptionContext

	var parent_command: nullable Command = null
	var sub_commands = new Array[Command]

	fun add_option(option: Option) do
		options.add_option(option)
	end

	fun add_command(command: Command) do
		command.parent_command = self
		sub_commands.add command
	end

	fun full_name: String do
		var parent = parent_command
		if parent == null then return name
		return "{parent.full_name} {name}"
	end

	fun usage do
		print "usage: {full_name}"
		print "\n{description}\n"
		if options.options.not_empty then
			print "Options:"
			options.usage
		end
		if sub_commands.not_empty then
			print "Sub-commands:"
			for cmd in sub_commands do
				print "\t{cmd.name}\t{cmd.description}"
			end
		end
		exit 1
	end

	fun parse(argv: Collection[String]) do
		options.parse(argv)

		var args = options.rest
		if args.not_empty then
			for cmd in sub_commands do
				if cmd.name == args.first then
					cmd.parse(args.subarray(1, args.length - 1))
					return
				end
			end
		end

		run(args)
	end

	fun run(args: Collection[String]) do
		usage
	end
end
