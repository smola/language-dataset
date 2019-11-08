author: Carlos Camacho
title: "[overcloud]: CREATE_FAILED Resource CREATE failed: The Referenced Attribute (ControllerServiceChain role_data) is incorrect."
related-bug: 
err: |
  [overcloud]: CREATE_FAILED Resource CREATE failed:
  The Referenced Attribute (ControllerServiceChain role_data)
  is incorrect.
sol: |
  This error appears sometines when a parameter is referenced
  but not declared in a template file.
  For example in a template file is calling {get_param: AodhPassword}
  in the parameters section of the same template it must be defined.
  Also a service template can be referencing a parameter like 
  aodh::db::mysql::password: {get_param: AodhPassword}
  defined in the base file.. This wont work as in the
  base file needs to be like:

  .
  .
  parameters:
  .
  .
    AodhPassword:
      description: The password for the aodh services.
      type: string
      hidden: true
  .
  .
  outputs:
    aux_parameters:
      description: Additional parameters referenced outside the base file
      value:
        aodh_password: {get_param: AodhPassword}
    role_data:
  .
  . 

  And in the service file

  .
  .
  resources:
    AodhBase:
      type: ./aodh-base.yaml
      properties:
        EndpointMap: {get_param: EndpointMap}
  .
  .
  outputs:
    role_data:
      description: Role data for the Aodh API service.
      value:
        service_name: aodh_api
        config_settings:
          map_merge:
             .
             . 
             aodh::api::keystone_password: {get_attr: [AodhBase, aux_parameters, aodh_password]}
             .
             .
             
