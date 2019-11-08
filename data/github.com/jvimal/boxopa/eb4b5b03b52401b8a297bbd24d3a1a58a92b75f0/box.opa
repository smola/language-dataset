
import stdlib.themes.bootstrap
import stdlib.upload
import stdlib.io.file
import stdlib.web.client

resources_css = @static_resource_directory("resources")

type FileInfo = {
  name: string;
  id: string;
  mimetype: string;
}

type File = {
  name: string;
  id: string;
  mimetype: string;
  content: string
}

type Box = { files: list(File) }

db /box: stringmap(Box)
db /box[_] = { files = [] }

@server_private
hostname() = "http://localhost:8080"

get_file_info(f: File) =
  { name = f.name;
    mimetype = f.mimetype; 
    id = f.id }

box_url(id) = "{hostname()}/box/{id}"

index_page() = 
(
  id = Random.string(8)
  Resource.styled_page("Creating new box", ["/css"],
    <body>
      <div id="content">
        <h1>Welcome</h1>
        <a href="/box/{id}"><img src="http://i.imgur.com/WBbSg.png"/></a>
        <h3>Your box has been created.  Click <a href="/box/{id}">the box</a> to open it!</h3>
		       <input type="text" id="perm" value="{box_url(id)}" onclick={_ -> Dom.select(#perm)} />
      </div>
    </body>
  )
)

@server_private
create_file(bid, f) =
(
  do /box[bid]/files <- List.add(f, /box[bid]/files);
  void
)

@server_private
delete_file(bid, id) =
(
  files = /box[bid]/files
  room = network(bid)
  info = {id = id; name = ""; mimetype = ""}
  do /box[bid]/files <- List.remove_p((e -> e.id == id), files)
  Network.broadcast(info, room)
)

get_image(m) =
  if String.has_prefix("image", m) then "http://i.imgur.com/wCvgr.png"
  else "http://i.imgur.com/qDkLr.png"

show_file(box, f) =
(
  <div class="span3" id="{f.id}" style="padding-top: 50px;">
    <a style="font-size: 1.5em;" href="/assets/{box}/{f.id}/{f.name}">{f.name}</><br/>
    <a href="/assets/{box}/{f.id}/{f.name}"><img class="thumbnail" style="width: 90px;" src="{get_image(f.mimetype)}"/></a><br/>
    <a href="#" class="btn danger" onclick={_ -> delete_file(box, f.id)}>Delete file</a>
  </div>
)

@server_private
process_upload(bid,upload_data) =
(
  up_file = StringMap.get("upload", upload_data.uploaded_files)
  match up_file with
    | {some = f} -> 
          id = Random.string(8)
          name = f.filename
          mtype = f.mimetype
          content = f.content
          room = network(bid)
          info = { id = id;
                   name = name;
                   mimetype = mtype; }
          new_file = { id = id;
                       name = name;
                       mimetype = mtype;
                       content = content } // Storing file in database; work on C-extension
          do create_file(bid, new_file)
          do Dom.remove(#upload)
          do Network.broadcast(info, room)
          void
    | _ -> 
          do Dom.remove(#upload)
          do Dom.transform([#error +<- <p>Error uploading file!</p>])
          void
)

show_upload_form(bid) =
(
  Upload.html(
    { Upload.default_config() with
        form_id = "upload";
        form_body =
            <input type="file" name="upload" />
            <input type="submit" class="btn" value="Upload!" />;
        process = a -> process_upload(bid, a);
     })
)


add_file(bid) =
(
  Dom.transform([#up <- show_upload_form(bid)])
)

network(id) : Network.network(FileInfo) =
  Network.cloud(id)

files_update(boxid, f: FileInfo) =
  if f.name != "" then
    Dom.transform([#files +<- show_file(boxid, f)])
  else
    Dom.remove(#{f.id})


@server_private
show_box(path) = 
(
  b = /box[path]
  room = network(path)
  callback = e -> files_update(path, e)
  finfo = List.map(get_file_info, b.files)
  Resource.styled_page("Showing box {path}", ["/css"],
    <body onready={_ -> Network.add_callback(callback, room)}>
      <div id="content">
        <h1>This is your box.  Upload anything you want!</h1>
        <h3>Click the file icon to download the file.</h3>
        <h3>Anyone with this box's URL will be able to download these files!</h3>
        <h3>Copy the URL and share with friends!</h3>
		       <input type="text" id="perm" value="{box_url(path)}" onclick={_ -> Dom.select(#perm)} />
        <p>All viewers of this page will see the files the instant they are uploaded.</p>
        <div class="row" id="files">
          {List.map(show_file(path,_), finfo)}
        </div>
        <div id="up">
        </div>
        <div id="error">
        </div>
        <a class="btn success" href="#" onclick={_ -> add_file(path)}>Add file</a>
      </div>
    </body>
  )
)

do_404() = 
(
  Resource.styled_page("Oops", [],
    <h1>Oops, we cannot find your page!</h1>
  )
)

deliver_assets(lst) =
  match lst with
    | [boxid, assetid, name] -> (
        files = /box[boxid]/files
        match List.find((e -> e.id == assetid && e.name == name), files) with
          | {some = file} -> Resource.raw_response(file.content, file.mimetype, {success})
          | _ -> Resource.raw_status({unauthorized})
      )
    | _ -> do_404()

start(uri) = (
  match uri with
    | {path = {nil} ...} -> index_page()
    | {path = {hd="box" ~tl} ...} -> show_box(String.concat("", tl))
    | {path = {hd="assets" ~tl} ...} -> deliver_assets(tl)
    | {path = {hd="css" ...} ...} -> 
          Resource.source(@static_source_content("resources/main.css"),"text/css")
    | {path = _ ...} -> do_404()
)

server = Server.simple_dispatch(start)

