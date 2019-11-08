# GodotHub Client Class
# Author: Nik Mirza
# Email: nik96mirza[at]gmail.com
class_name GodotHub

signal error(err) # Declare signals
#signal listening
signal connected
signal join(id)
signal left(id)
signal message(data)
signal ping(ping)

var conn

var server = {
	port = 5000,
	host = '127.0.0.1',
}

var client = {
	ID = "",
	channel = 'global'
}

func _init(serverport = 5000, serverhost = '127.0.0.1', serverchannel= "global", listenport = 4000):
	server.host = serverhost
	server.port = serverport
	client.channel = serverchannel
	
	conn = PacketPeerUDP.new()
	conn.set_dest_address(server.host,server.port);
	
	var err
	while err:
		err = conn.listen(listenport)
		if err:
			listenport += 1;
			emit_signal("error", err)
			continue
		break;
		
	send_data({event="connecting"})
	
func is_listening():
	if !conn.is_listening():
		return false
		
	if data_available():
		var data = get_data()

		match data.event:
			"connected":
				client.ID = data.ID
				emit_signal("connected")
				return
			
			"join":
				unicast({event="join",msg=String(client.ID)+" join the channel",ID=client.ID},data.ID)#send join to newly joined client
				emit_signal("join", data.ID)# join signal when data is received
				return
			
			"left":
				emit_signal("left", data.ID)#left signal when data is received
				return
				
			"ping":
				var ping = OS.get_ticks_msec() - data.data
				ping = round(ping)
				emit_signal("ping",ping)
				return
			_:
				if(data.data.event == "join"):
					emit_signal("join", data.data.ID);
				else:
					emit_signal("message",data.data)#message signal when data is received
		
func change_channel(channel):
	client.channel = channel
	var dat = { event = "channel"};
	dat.data = { ID = client.ID, channel = channel };
	send_data(dat);
	
func ping():
	send_data({event="ping", data=OS.get_ticks_msec()})
	
func data_available():
	return conn.get_available_packet_count() > 0
	
func get_data():#As dictionary
	var data = conn.get_var()
	var dict = {}
	dict = JSON.parse(data).result
	return dict
	
func broadcast(data): #Only accept dictionary
	var dat = {event="broadcast"}
	dat.data = data
	dat.ID = client.ID
	send_data(dat)
	
func multicast(data): #Only accept dictionary
	var dat = {event="multicast"}
	dat.data = data
	dat.ID = client.ID
	send_data(dat)
	
func unicast(data, clientID): #Only accept dictionary
	var dat = {event="unicast"}
	dat.data = data
	dat.ID = clientID
	send_data(dat)
	
func send_data(data): #Only accept dictionary
	client.data = data
	conn.put_var(JSON.print(client))
		
func disconnect_server():
	send_data({event="disconnect"})
	conn.close()