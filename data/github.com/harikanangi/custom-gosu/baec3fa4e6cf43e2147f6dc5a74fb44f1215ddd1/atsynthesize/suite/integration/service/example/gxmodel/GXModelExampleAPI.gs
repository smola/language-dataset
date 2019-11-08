//
//  GXModelExampleAPI.gs
//
//  Created by Hari Kanangi on 01/25/2013.
//  Copyright (c) 2013 Hari Kanangi. 
//  License: http://www.harikanangi.com/license.txt
//
package atsynthesize.suite.integration.service.example.gxmodel

uses gw.api.database.Query
uses gw.api.server.AvailabilityLevel
uses gw.api.webservice.exception.BadIdentifierException
uses gw.api.webservice.exception.SOAPException
uses gw.xml.ws.annotation.WsiAvailability
uses gw.xml.ws.annotation.WsiWebService

uses atsynthesize.suite.integration.service.WsiServiceBaseAPI
uses atsynthesize.suite.integration.service.example.gxmodel.gxuser.User
uses atsynthesize.suite.integration.service.exception.InvalidFieldsException

@WsiWebService( "http://atsynthesize.com/gw/ws/atsynthesize/integration/service/example/gxmodel/GXModelExampleAPI" )
@WsiAvailability(AvailabilityLevel.MULTIUSER)
class GXModelExampleAPI 
        extends WsiServiceBaseAPI {
  
  /**
   * Constructor - Invokes the super's constructor with GXModelExampleAPI
   * as the name of the inbound integration. The log file for this will be
   * determined by the configuration of Integration.GXModelExampleAPI or
   * Integration categories in logging.properties
   */  
  construct() {
    super("GXModelExampleAPI")
  }
  
  /**
   * Example of an Inbound WS-I Web Service that takes in a single instance
   * of a GX Model entity User and returns an instance of the GX Model entity
   * User. This method in turn calls the base class wsiExecute method
   * and passes in a block that invokes the private method 
   * processExampleGxModelGetUserWsiService
   *  
   * @param pInput - Input Parameter - User Entity - GX Model Instance that
   * contains the public id of the user to be queried for and retrieved.
   * 
   * @return User GX Model instance - Returns back the User Information for 
   * the public id specified to the calling WSI Service Client
   * 
   * @Throws InvalidFieldsException
   * @Throws BadIdentifierException
   * @Throws SOAPException
   * 
   */  
  @Throws(InvalidFieldsException, "Invalid values passed in")
  @Throws(BadIdentifierException, "Identifier passed in is not found/incorrect")
  @Throws(SOAPException, "SOAP Exceptions includes Expected And Unexpected Exceptions")
  public function exampleGxModelGetUserWsiService(pInput : User) 
                    : User {
    
    var result : User = null
    result =  wsiExecute(
                \ requestInput : User -> { 
                    return  processExampleGxModelGetUserWsiService(requestInput)
                }, 
                pInput
              )
    return result
  }

  /**
   * Example of an Inbound WS-I Web Service that takes in no input returns 
   * an array of instances of the GX Model entity User. This method in turn 
   * calls the base class wsiExecute method and passes in a block that invokes 
   * the private method processExampleGxModelGetAllUsersWsiServiceNoInputArrayOutput
   *  
   * @return User[] GX Model - Returns back all the configured User Information
   * 
   * @Throws InvalidFieldsException
   * @Throws BadIdentifierException
   * @Throws SOAPException
   * 
   */  
  @Throws(InvalidFieldsException, "Invalid values passed in")
  @Throws(BadIdentifierException, "Identifier passed in is not found/incorrect")
  @Throws(SOAPException, "SOAP Exceptions includes Expected And Unexpected Exceptions")
  public function exampleGxModelGetAllUsersWsiServiceNoInputArrayOutput() 
                    : User[] {
    
    var result : User[] = null
    result =  wsiExecuteAndReturnArray(
                \ requestInput : User -> { 
                    return  processExampleGxModelGetAllUsersWsiServiceNoInputArrayOutput()
                }, 
                null as User
              )
    return result
  }

  /**
   * Mock Business Implementation - Retrieves User Information given a public id
   *  
   * @param pInput - Input Parameter - User Entity - GX Model Instance that
   * contains the public id of the user to be queried for and retrieved.
   * 
   * @return User GX Model instance - Returns back the User Information for 
   * the public id specified to the calling WSI Service Client
   * 
   */  
  protected function processExampleGxModelGetUserWsiService(pInput : User) 
                    : User {
    var result : User = null
                      
    if (pInput == null || !pInput.PublicID.HasContent) {
      throw new InvalidFieldsException("Invalid/Null Input passed")
    }
    
    var usr = Query.make(entity.User)
                   .compare("PublicID", equals, pInput.PublicID)
                   .select()
                   .AtMostOneRow
    if (usr != null) {
      result = new User(usr)
    }
    
    return result
  }

  /**
   * Mock Business Implementation - Returns all Users
   *  
   * @return User[] GX Model - Returns back all the configured User Information
   * 
   * @Throws InvalidFieldsException
   * @Throws BadIdentifierException
   * @Throws SOAPException
   * 
   */  
  protected function processExampleGxModelGetAllUsersWsiServiceNoInputArrayOutput() 
                    : User[] {
    var result : User[] = null
                      
    var usrs = Query.make(entity.User)
                    .select()
    if (usrs.Count > 0) {
      result = new User[usrs.Count]
      usrs.eachWithIndex(\ u, i -> {
                          var gxUser = new User(u)
                          result[i] = gxUser
      })
    }
    
    return result
  }
}