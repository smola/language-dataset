(*
Module: Test_Nginx
  Provides unit tests and examples for the <Nginx> lens.
*)
module Test_nginx = 

    let conf ="test a;
user nginx nginx;
worker_connections 1024;
"
    test Nginx.lns get conf =
       { "test"  = "a" }
       { "user"  = "nginx nginx" }
       { "worker_connections"  = "1024" }


          let block = "events {
        worker_connections 1024;
        test a;
}
"
    test Nginx.lns get block =
    { "events"
      { "worker_connections" = "1024" }
      { "test" = "a" }
    }

    let nested_server ="
http {
  server {
    test a;
  }
  server {
    test a;
  }
}
"
    test Nginx.lns get nested_server =
    {}
    { "http"
      { "server"
        { "1"
          { "test" = "a" }
        }
      }
    }


    let nested_servers ="
http {
  server {
    test a;
  }
  server {
    test a;
  }
}
"
    test Nginx.lns get nested_servers =
    {}
    { "http"
      { "server"
        { "1"
          { "test" = "a" }
        }
        { "2"
          { "test" = "a" }
        }
      }
    }

    let nested_server_location ="
http {
  server {
    location {
      test a;
    }
  }
}
"
    test Nginx.lns get nested_server_location =
    {}
    { "http"
      { "server"
        { "1"
          { "location"
            { "test" = "a" }
          }
       }
      }
    }
