note
	description: "[
						This class is basically a PACKET with an additional field peer_address which is used by the UDP_SENDER_THREAD
						so that it knows who to send the message to. Furthermore it has some helper features that ease the creation of a packet
						such as fill. For every message_type there exists an appropriate creation feature so that in the CONNECTION_MANAGER
						only create packet.make_register_packet("Anna"); send_queue.extend(packet) has to be called.
				 ]"
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	TARGET_PACKET

inherit
	PACKET

create
	make_register_packet, make_query_packet, make_keep_alive_packet, make_unregister_packet,
	make_application_packet, make_registered_users_packet, make_hole_punch_packet

feature -- Initialization

	make_register_packet (my_name: STRING; a_p2p_setup: P2P_SETUP)
		do
			create json_object.make

				-- create message type
			put_type ({P2P_PROTOCOL_CONSTANTS}.register_message)

				-- create client name
			put_string ({P2P_PROTOCOL_CONSTANTS}.name_key, my_name)

				-- fill the packet
			fill

				-- set peer_address
			create peer_address.make_from_hostname_and_port (a_p2p_setup.server_ip, a_p2p_setup.server_port)
		end

	make_query_packet (peer_name: STRING; a_p2p_setup: P2P_SETUP)
		do
			create json_object.make

				-- create message type
			put_type ({P2P_PROTOCOL_CONSTANTS}.query_message)

				-- create peer_name to query
			put_string ({P2P_PROTOCOL_CONSTANTS}.name_key, peer_name)

				-- fill the packet
			fill

				-- set peer_address
			create peer_address.make_from_hostname_and_port (a_p2p_setup.server_ip, a_p2p_setup.server_port)

		end

	make_keep_alive_packet (a_peer_address: NETWORK_SOCKET_ADDRESS)
		do
			create json_object.make

				-- create message type
			put_type ({P2P_PROTOCOL_CONSTANTS}.keep_alive_message)

				-- fill the packet
			fill

				-- set peer_address
			create peer_address.make_from_address_and_port (a_peer_address.host_address, a_peer_address.port)

		end

	make_unregister_packet (a_name: STRING; a_p2p_setup: P2P_SETUP)

		do
			create json_object.make

				-- create message type
			put_type ({P2P_PROTOCOL_CONSTANTS}.unregister_message)

				-- create client name
			put_string ({P2P_PROTOCOL_CONSTANTS}.name_key, a_name)

				-- fill the packet
			fill

				-- set peer_address
			create peer_address.make_from_hostname_and_port (a_p2p_setup.server_ip, a_p2p_setup.server_port)

		end

	make_application_packet (a_peer_address: NETWORK_SOCKET_ADDRESS; message: STRING)

		do
			create json_object.make

				-- create message type
			put_type ({P2P_PROTOCOL_CONSTANTS}.application_message)

				-- create message
			put_string ({P2P_PROTOCOL_CONSTANTS}.data_key, message)

				-- fill the packet
			fill

				-- set peer_address
			create peer_address.make_from_address_and_port (a_peer_address.host_address, a_peer_address.port)
		end

	make_registered_users_packet (a_p2p_setup: P2P_SETUP)

		do
			create json_object.make

				-- create message type	
			put_type ({P2P_PROTOCOL_CONSTANTS}.registered_users_message)

				-- fill the packet
			fill

				-- set peer_address
			create peer_address.make_from_hostname_and_port (a_p2p_setup.server_ip, a_p2p_setup.server_port)
		end

	make_hole_punch_packet (a_peer_address: NETWORK_SOCKET_ADDRESS)
		do
			create json_object.make

				-- create message type
			put_type ({P2P_PROTOCOL_CONSTANTS}.hole_punch_message)

				-- fill the packet
			fill

				-- set peer_address
			create peer_address.make_from_address_and_port (a_peer_address.host_address, a_peer_address.port)
		end

feature -- helpers

	fill
		local
			string_rep:  STRING
			i: INTEGER
		do
			string_rep := json_object.representation
			make (string_rep.count)
			from i := 1
			until i > string_rep.count
			loop
				put_element (string_rep.item (i), i-1)
				i := i+1
			end
		end

	put_string (key: STRING value: STRING)
		local
			j_key: JSON_STRING
			j_value: JSON_STRING
		do
			create j_key.make_from_string (key)
			create j_value.make_from_string (value)
			json_object.put (j_value, j_key)
		end

	put_integer (key: STRING value: INTEGER_64)
		local
			j_key: JSON_STRING
			j_value: JSON_NUMBER
		do
			create j_key.make_from_string (key)
			create j_value.make_integer (value)
			json_object.put (j_value, j_key)
		end

	put_type (type: INTEGER_64)
		do
			put_integer({P2P_PROTOCOL_CONSTANTS}.message_type_key, type)
		end

feature -- DATA

	json_object: JSON_OBJECT

feature -- TARGET

	peer_address: NETWORK_SOCKET_ADDRESS

end
