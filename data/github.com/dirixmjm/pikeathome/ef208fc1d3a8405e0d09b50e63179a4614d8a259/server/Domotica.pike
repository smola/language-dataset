#include <module.h>
#define DDEBUGSWITCHBOARD
inherit Base_func;

protected mapping modules = ([]);
protected array loggers = ({});
protected array dataloggers = ({});
//object xmlrpc;
object ICom;
protected object server_configuration;
protected mapping run_config;
string name ="";


void create( mapping rconfig)
{
   run_config = rconfig;
   name = run_config->name;
   server_configuration = Config( run_config->database, run_config->CurrentDBVersion, name );

   //FIXME, should this be here?
   server_configuration->listenaddress = run_config->listenaddress;
   ICom = .InterCom( name,switchboard,server_configuration ); 
   if ( server_configuration->module )
      moduleinit( server_configuration->module );
   if ( server_configuration->logmem )
      call_out(LogMemory,60);
}

//Array with server configuration parameters
//Goal is to keep it to a minimum and let modules worry about operation.
array ServerParameters = ({
                   ({ "logmem",PARAM_BOOLEAN,0,"Turn On / Off Memory Logging",POPT_NONE }),
		   ({ "logoutput", PARAM_MODULELOGDATA,"","Global Data logging module",0 }),

});

void LogMemory()
{
   if ( server_configuration->logmem == 1 )
   {
      int tstamp = time(1);
      foreach(_memory_usage();string key; int value)
      {
         if(has_suffix(key,"_bytes"))
            value=value/1024;
         call_out(switchboard,0,name,server_configuration->logoutput,COM_LOGDATA,
         (["name":name+"."+key,"stamp":tstamp,"data":value]) );
      }
   }
   call_out(LogMemory,60);
}

void logout(int log_level, mixed ... args )
{
   Stdio.stdout.write(@args);
}

void rpc_command( string sender, string receiver, int command, mapping parameters )
{
   array split = split_server_module_sensor_value(receiver);
   switch( command )
   {
      case COM_PARAM:
      {
        if ( parameters && mappingp(parameters) )
        {
            foreach(ServerParameters, array var)
            {
               if( has_index( parameters, var[0] ) )
                  server_configuration[var[0]] = parameters[var[0]];
            }
         }
         array ret = ({});
         foreach(ServerParameters, array var)
            ret+= ({ var + ({ server_configuration[var[0]] }) });
         switchboard(name, sender, -command, ret );
      }
      break;
      case COM_FIND:
      {
         array failed_modules=({});
         array compiled_modules = ({});
         object moddir = Filesystem.Traversion(run_config->installpath + "/modules" );
         foreach( moddir; string dirname; string filename )
         { 
            string name="";
            if( !has_suffix(filename,".pike")) continue;
            sscanf(filename,"%s\.pike",name);
            object themodule;
            //FIXME should a module be instantiated here, probably for parameters
            mixed catch_result = catch { 
               themodule =compile_file(dirname+filename)(name,UNDEFINED,UNDEFINED);
            };
            if(catch_result)
            {
               failed_modules += ({ ([  "name":name,
                            "error": "Compilation Failed" ]) });
               logerror("Error:%O\n",catch_result);
            }
            else
            {
               compiled_modules += ({ ([ "name":name,
                             "parameters":themodule->ModuleParameters +
                             ({ ({  "filename",PARAM_RO,filename,"Module File", POPT_NONE }) })
                              ]) });
            }
         }
         switchboard(name, sender, -command, compiled_modules + failed_modules );
      }
      break;
      case COM_LIST:
      {
         array modulenames=({});
         foreach( indices(modules), string module)
         {
            modulenames += ({ ([ "name":module ]) });
         }
         switchboard(name, sender, -command, modulenames );
      }
      break;
      case COM_ALLSENSOR:
      {
         array sensors=({});
         foreach( indices(modules), string module)
         {
           if( ! (modules[module]->module_type & MODULE_SENSOR) )
              continue;
           foreach( values(modules[module]->sensors), object sensor )
              sensors+=({ ([ "name":sensor->SensorProperties->name ]) });
         }
         switchboard(name, sender , -command, sensors);
      }
      break;
      case COM_ADD:
      {
         string module_name = name+"."+parameters->name;
         mapping params = parameters->parameters + ([]);
         //m_delete(parameters,"name");
         if ( has_value(server_configuration->module + ({}), module_name ) )
         {
            string error=sprintf("There already exists a module instance with name %s\n",module_name);
            switchboard(module_name, sender, 30, ([ "error":error ]));
         }
         server_configuration->module+=({module_name});
         object cfg = server_configuration->Configuration( module_name );
         //FIXME if parameter not given set default parameter?
         foreach ( params; string index; mixed value )
           cfg[index]=value;
         moduleinit(({ module_name } ) );
         switchboard(name, sender, -command, UNDEFINED );
      }
      break;
      case COM_DROP:
      {
         if( !has_index(modules, parameters->name ))
           switchboard(name, sender, 30, (["error": sprintf("Can't Delete unknown module %s",parameters->name) ]) );
         modules[parameters->name]->close();
         m_delete(modules,parameters->name);
         server_configuration->module -= ({ parameters->name });
         m_delete(server_configuration, parameters->name );
      }
      break;
      case COM_ERROR:
         call_out(switchboard, 0, name, name, COM_LOGEVENT, ([ "level":LOG_ERR, "error":parameters->error ]) );
      break;
      case COM_LOGEVENT:
      foreach(loggers, string logger)
      {
         //Fixme Switchboard?
         modules[logger]->log_event( parameters->level, sender, parameters->error );
#ifdef DEBUG
       logout(parameters->level,parameters->error);
#endif
      }
      break;
      case COM_LOGDATA:
      {
         //Log only to global log module
         //FIXME Create internal logging
         if( has_index(server_configuration,"logoutput") 
                       && server_configuration->logoutput != "internal" )
           switchboard(sender,server_configuration->logoutput, COM_LOGDATA, parameters);
      }
      break;
      case COM_RETRLOGDATA:
      {
         //Retrieve log from global log module
         if( has_index(server_configuration,"logoutput") 
                       && server_configuration->logoutput != "internal" )
           switchboard(sender,server_configuration->logoutput, COM_RETRLOGDATA, parameters);
      }
      break;
      default:
      switchboard(name, sender, COM_ERROR, ([ "error":sprintf("Unknown Command %d for server",command) ]) );
   }
}

/*
* There are two ways to get values from an sensor 
* Either a direct call to the switchboard with receiver and sender
* or add a hook to a given sensor / variable and receive it everytime
* the variable enters the switchboard
*/
int logcount=0;

void switchboard( string sender, string receiver, int command, mixed parameters )
{
   //A receiver should always be given
   if( !receiver || !sizeof(receiver ))
   {
      call_out(switchboard, 0, name, sender, COM_ERROR, ([ "error":"No module,sensor or value is requested\n" ]) );
   }

#ifdef DEBUGSWITCHBOARD
         logout(LOG_DEBUG,"Switchboard %d %O\n",++logcount, parameters );
         logout(LOG_DEBUG,"Switchboard %d received command %d for %s from %s\n",logcount,command,receiver, sender);
#endif

   array split = cumulative_split_server_module_sensor_value(receiver);
   // ({ server, server.module,server.module.sensor,server.module.sensor.value})
   //Something went wrong and the switchboard is called
   //Switchboard message for the current server
   if ( split[0] == name || split[0]=="broadcast")
   {
      //Message for the server

      //Propagate to a module
      if( sizeof( split) > 1)
      {
         if ( ! has_index(modules,split[1]) )
         {
            call_out(switchboard, 0, name, sender, 30, ([ "error":sprintf("Module %s not found",split[1]) ]) );
         }
         else
         {
         //Call the requested module
            call_out(modules[split[1]]->rpc_command, 0, sender, receiver, command, parameters );
         }
      }
      //Command for the server
      else
         call_out(rpc_command, 0, sender, receiver, command, parameters );
      
   }
   else
   {
      //Message is for a different server
      call_out(ICom->rpc_command, 0, sender, receiver, command, parameters );
   }


     
}

void moduleinit( array names )
{
   foreach(names, string name)
   {
      object mod_conf = server_configuration->Configuration(name);
      if ( has_index( mod_conf, "debug" ) && (int) mod_conf->debug == 1 )
         master()->CompatResolver()->add_predefine(upper_case(name)+"DEBUG","1");
      else
         master()->CompatResolver()->remove_predefine(upper_case(name)+"DEBUG");
      object themodule;
      mixed catch_result = catch {
                    
         themodule = compile_file(run_config->installpath + "/modules/" + mod_conf->filename)( name, mod_conf, switchboard );
       

      };
      if(catch_result)
      {
         logerror("Error Module %s Compilation Failed\n",name);
         continue;
      }
      else
      {
         themodule->init(); 
         modules+= ( [name: themodule ]);
      } 
      //Cache loggers, so they don't have to be search for every log.
      if( modules[name]->module_type & MODULE_LOGEVENT )
         loggers+= ({ name });
      if( modules[name]->module_type & MODULE_LOGDATA )
         dataloggers+= ({ name });
   }

}

void close()
{
   foreach(values(modules), object module)
   {
      module->close();
      destruct(module);
   }
}

/*
* Helper / Short functions for Modules
*/

void logdebug(mixed ... args)
{
   switchboard(name, name, COM_LOGEVENT, ([ "level":LOG_DEBUG, "error":sprintf(@args) ]) );
}

void logerror(mixed ... args)
{
   switchboard(name, name, COM_LOGEVENT, ([ "level":LOG_ERR, "error":sprintf(@args) ]) );

}
