  MEMBER('existencias.clw')

  INCLUDE('ABFILE.INC'),ONCE

  MAP
existencias_BC0:DctInit    PROCEDURE
existencias_BC0:DctKill    PROCEDURE
existencias_BC0:FilesInit  PROCEDURE
  END

Hide:Access:Proveedores CLASS(FileManager),TYPE            ! FileManager for Proveedores
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Proveedores CLASS(RelationManager),TYPE        ! RelationManager for Proveedores
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Transportistas CLASS(FileManager),TYPE         ! FileManager for Transportistas
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Transportistas CLASS(RelationManager),TYPE     ! RelationManager for Transportistas
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Plantas  CLASS(FileManager),TYPE               ! FileManager for Plantas
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Plantas  CLASS(RelationManager),TYPE           ! RelationManager for Plantas
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Existencias CLASS(FileManager),TYPE            ! FileManager for Existencias
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Existencias CLASS(RelationManager),TYPE        ! RelationManager for Existencias
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Descargas CLASS(FileManager),TYPE              ! FileManager for Descargas
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Descargas CLASS(RelationManager),TYPE          ! RelationManager for Descargas
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Viajes   CLASS(FileManager),TYPE               ! FileManager for Viajes
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Viajes   CLASS(RelationManager),TYPE           ! RelationManager for Viajes
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Densidades_Corregidas CLASS(FileManager),TYPE  ! FileManager for Densidades_Corregidas
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Densidades_Corregidas CLASS(RelationManager),TYPE ! RelationManager for Densidades_Corregidas
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Niveles_Volumenes CLASS(FileManager),TYPE      ! FileManager for Niveles_Volumenes
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Niveles_Volumenes CLASS(RelationManager),TYPE  ! RelationManager for Niveles_Volumenes
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:t_tanques CLASS(FileManager),TYPE              ! FileManager for t_tanques
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:t_tanques CLASS(RelationManager),TYPE          ! RelationManager for t_tanques
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Tanques_plantas CLASS(FileManager),TYPE        ! FileManager for Tanques_plantas
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Tanques_plantas CLASS(RelationManager),TYPE    ! RelationManager for Tanques_plantas
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Presiones_corregidas CLASS(FileManager),TYPE   ! FileManager for Presiones_corregidas
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Presiones_corregidas CLASS(RelationManager),TYPE ! RelationManager for Presiones_corregidas
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Localidades_GLP CLASS(FileManager),TYPE        ! FileManager for Localidades_GLP
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Localidades_GLP CLASS(RelationManager),TYPE    ! RelationManager for Localidades_GLP
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Mediciones CLASS(FileManager),TYPE             ! FileManager for Mediciones
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Mediciones CLASS(RelationManager),TYPE         ! RelationManager for Mediciones
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:aux_sql  CLASS(FileManager),TYPE               ! FileManager for aux_sql
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:aux_sql  CLASS(RelationManager),TYPE           ! RelationManager for aux_sql
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:programacion CLASS(FileManager),TYPE           ! FileManager for programacion
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:programacion CLASS(RelationManager),TYPE       ! RelationManager for programacion
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Procedencias CLASS(FileManager),TYPE           ! FileManager for Procedencias
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Procedencias CLASS(RelationManager),TYPE       ! RelationManager for Procedencias
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Mediciones_aux CLASS(FileManager),TYPE         ! FileManager for Mediciones_aux
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Mediciones_aux CLASS(RelationManager),TYPE     ! RelationManager for Mediciones_aux
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:parametros CLASS(FileManager),TYPE             ! FileManager for parametros
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:parametros CLASS(RelationManager),TYPE         ! RelationManager for parametros
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:PlantasStock CLASS(FileManager),TYPE           ! FileManager for PlantasStock
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:PlantasStock CLASS(RelationManager),TYPE       ! RelationManager for PlantasStock
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:viajes_anticipos CLASS(FileManager),TYPE       ! FileManager for viajes_anticipos
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:viajes_anticipos CLASS(RelationManager),TYPE   ! RelationManager for viajes_anticipos
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END


_Hide:Access:Proveedores &Hide:Access:Proveedores,AUTO,THREAD
_Hide:Relate:Proveedores &Hide:Relate:Proveedores,AUTO,THREAD
_Hide:Access:Transportistas &Hide:Access:Transportistas,AUTO,THREAD
_Hide:Relate:Transportistas &Hide:Relate:Transportistas,AUTO,THREAD
_Hide:Access:Plantas &Hide:Access:Plantas,AUTO,THREAD
_Hide:Relate:Plantas &Hide:Relate:Plantas,AUTO,THREAD
_Hide:Access:Existencias &Hide:Access:Existencias,AUTO,THREAD
_Hide:Relate:Existencias &Hide:Relate:Existencias,AUTO,THREAD
_Hide:Access:Descargas &Hide:Access:Descargas,AUTO,THREAD
_Hide:Relate:Descargas &Hide:Relate:Descargas,AUTO,THREAD
_Hide:Access:Viajes  &Hide:Access:Viajes,AUTO,THREAD
_Hide:Relate:Viajes  &Hide:Relate:Viajes,AUTO,THREAD
_Hide:Access:Densidades_Corregidas &Hide:Access:Densidades_Corregidas,AUTO,THREAD
_Hide:Relate:Densidades_Corregidas &Hide:Relate:Densidades_Corregidas,AUTO,THREAD
_Hide:Access:Niveles_Volumenes &Hide:Access:Niveles_Volumenes,AUTO,THREAD
_Hide:Relate:Niveles_Volumenes &Hide:Relate:Niveles_Volumenes,AUTO,THREAD
_Hide:Access:t_tanques &Hide:Access:t_tanques,AUTO,THREAD
_Hide:Relate:t_tanques &Hide:Relate:t_tanques,AUTO,THREAD
_Hide:Access:Tanques_plantas &Hide:Access:Tanques_plantas,AUTO,THREAD
_Hide:Relate:Tanques_plantas &Hide:Relate:Tanques_plantas,AUTO,THREAD
_Hide:Access:Presiones_corregidas &Hide:Access:Presiones_corregidas,AUTO,THREAD
_Hide:Relate:Presiones_corregidas &Hide:Relate:Presiones_corregidas,AUTO,THREAD
_Hide:Access:Localidades_GLP &Hide:Access:Localidades_GLP,AUTO,THREAD
_Hide:Relate:Localidades_GLP &Hide:Relate:Localidades_GLP,AUTO,THREAD
_Hide:Access:Mediciones &Hide:Access:Mediciones,AUTO,THREAD
_Hide:Relate:Mediciones &Hide:Relate:Mediciones,AUTO,THREAD
_Hide:Access:aux_sql &Hide:Access:aux_sql,AUTO,THREAD
_Hide:Relate:aux_sql &Hide:Relate:aux_sql,AUTO,THREAD
_Hide:Access:programacion &Hide:Access:programacion,AUTO,THREAD
_Hide:Relate:programacion &Hide:Relate:programacion,AUTO,THREAD
_Hide:Access:Procedencias &Hide:Access:Procedencias,AUTO,THREAD
_Hide:Relate:Procedencias &Hide:Relate:Procedencias,AUTO,THREAD
_Hide:Access:Mediciones_aux &Hide:Access:Mediciones_aux,AUTO,THREAD
_Hide:Relate:Mediciones_aux &Hide:Relate:Mediciones_aux,AUTO,THREAD
_Hide:Access:parametros &Hide:Access:parametros,AUTO,THREAD
_Hide:Relate:parametros &Hide:Relate:parametros,AUTO,THREAD
_Hide:Access:PlantasStock &Hide:Access:PlantasStock,AUTO,THREAD
_Hide:Relate:PlantasStock &Hide:Relate:PlantasStock,AUTO,THREAD
_Hide:Access:viajes_anticipos &Hide:Access:viajes_anticipos,AUTO,THREAD
_Hide:Relate:viajes_anticipos &Hide:Relate:viajes_anticipos,AUTO,THREAD


existencias_BC0:DctInit PROCEDURE
  CODE
  _Hide:Access:Proveedores &= NEW(Hide:Access:Proveedores)
  _Hide:Relate:Proveedores &= NEW(Hide:Relate:Proveedores)
  _Hide:Access:Transportistas &= NEW(Hide:Access:Transportistas)
  _Hide:Relate:Transportistas &= NEW(Hide:Relate:Transportistas)
  _Hide:Access:Plantas &= NEW(Hide:Access:Plantas)
  _Hide:Relate:Plantas &= NEW(Hide:Relate:Plantas)
  _Hide:Access:Existencias &= NEW(Hide:Access:Existencias)
  _Hide:Relate:Existencias &= NEW(Hide:Relate:Existencias)
  _Hide:Access:Descargas &= NEW(Hide:Access:Descargas)
  _Hide:Relate:Descargas &= NEW(Hide:Relate:Descargas)
  _Hide:Access:Viajes &= NEW(Hide:Access:Viajes)
  _Hide:Relate:Viajes &= NEW(Hide:Relate:Viajes)
  _Hide:Access:Densidades_Corregidas &= NEW(Hide:Access:Densidades_Corregidas)
  _Hide:Relate:Densidades_Corregidas &= NEW(Hide:Relate:Densidades_Corregidas)
  _Hide:Access:Niveles_Volumenes &= NEW(Hide:Access:Niveles_Volumenes)
  _Hide:Relate:Niveles_Volumenes &= NEW(Hide:Relate:Niveles_Volumenes)
  _Hide:Access:t_tanques &= NEW(Hide:Access:t_tanques)
  _Hide:Relate:t_tanques &= NEW(Hide:Relate:t_tanques)
  _Hide:Access:Tanques_plantas &= NEW(Hide:Access:Tanques_plantas)
  _Hide:Relate:Tanques_plantas &= NEW(Hide:Relate:Tanques_plantas)
  _Hide:Access:Presiones_corregidas &= NEW(Hide:Access:Presiones_corregidas)
  _Hide:Relate:Presiones_corregidas &= NEW(Hide:Relate:Presiones_corregidas)
  _Hide:Access:Localidades_GLP &= NEW(Hide:Access:Localidades_GLP)
  _Hide:Relate:Localidades_GLP &= NEW(Hide:Relate:Localidades_GLP)
  _Hide:Access:Mediciones &= NEW(Hide:Access:Mediciones)
  _Hide:Relate:Mediciones &= NEW(Hide:Relate:Mediciones)
  _Hide:Access:aux_sql &= NEW(Hide:Access:aux_sql)
  _Hide:Relate:aux_sql &= NEW(Hide:Relate:aux_sql)
  _Hide:Access:programacion &= NEW(Hide:Access:programacion)
  _Hide:Relate:programacion &= NEW(Hide:Relate:programacion)
  _Hide:Access:Procedencias &= NEW(Hide:Access:Procedencias)
  _Hide:Relate:Procedencias &= NEW(Hide:Relate:Procedencias)
  _Hide:Access:Mediciones_aux &= NEW(Hide:Access:Mediciones_aux)
  _Hide:Relate:Mediciones_aux &= NEW(Hide:Relate:Mediciones_aux)
  _Hide:Access:parametros &= NEW(Hide:Access:parametros)
  _Hide:Relate:parametros &= NEW(Hide:Relate:parametros)
  _Hide:Access:PlantasStock &= NEW(Hide:Access:PlantasStock)
  _Hide:Relate:PlantasStock &= NEW(Hide:Relate:PlantasStock)
  _Hide:Access:viajes_anticipos &= NEW(Hide:Access:viajes_anticipos)
  _Hide:Relate:viajes_anticipos &= NEW(Hide:Relate:viajes_anticipos)
  Relate:Proveedores &= _Hide:Relate:Proveedores
  Relate:Transportistas &= _Hide:Relate:Transportistas
  Relate:Plantas &= _Hide:Relate:Plantas
  Relate:Existencias &= _Hide:Relate:Existencias
  Relate:Descargas &= _Hide:Relate:Descargas
  Relate:Viajes &= _Hide:Relate:Viajes
  Relate:Densidades_Corregidas &= _Hide:Relate:Densidades_Corregidas
  Relate:Niveles_Volumenes &= _Hide:Relate:Niveles_Volumenes
  Relate:t_tanques &= _Hide:Relate:t_tanques
  Relate:Tanques_plantas &= _Hide:Relate:Tanques_plantas
  Relate:Presiones_corregidas &= _Hide:Relate:Presiones_corregidas
  Relate:Localidades_GLP &= _Hide:Relate:Localidades_GLP
  Relate:Mediciones &= _Hide:Relate:Mediciones
  Relate:aux_sql &= _Hide:Relate:aux_sql
  Relate:programacion &= _Hide:Relate:programacion
  Relate:Procedencias &= _Hide:Relate:Procedencias
  Relate:Mediciones_aux &= _Hide:Relate:Mediciones_aux
  Relate:parametros &= _Hide:Relate:parametros
  Relate:PlantasStock &= _Hide:Relate:PlantasStock
  Relate:viajes_anticipos &= _Hide:Relate:viajes_anticipos


existencias_BC0:FilesInit PROCEDURE
  CODE
  _Hide:Relate:Proveedores.Init
  _Hide:Relate:Transportistas.Init
  _Hide:Relate:Plantas.Init
  _Hide:Relate:Existencias.Init
  _Hide:Relate:Descargas.Init
  _Hide:Relate:Viajes.Init
  _Hide:Relate:Densidades_Corregidas.Init
  _Hide:Relate:Niveles_Volumenes.Init
  _Hide:Relate:t_tanques.Init
  _Hide:Relate:Tanques_plantas.Init
  _Hide:Relate:Presiones_corregidas.Init
  _Hide:Relate:Localidades_GLP.Init
  _Hide:Relate:Mediciones.Init
  _Hide:Relate:aux_sql.Init
  _Hide:Relate:programacion.Init
  _Hide:Relate:Procedencias.Init
  _Hide:Relate:Mediciones_aux.Init
  _Hide:Relate:parametros.Init
  _Hide:Relate:PlantasStock.Init
  _Hide:Relate:viajes_anticipos.Init


existencias_BC0:DctKill PROCEDURE
  CODE
  _Hide:Relate:Proveedores.Kill
  DISPOSE(_Hide:Relate:Proveedores)
  _Hide:Relate:Transportistas.Kill
  DISPOSE(_Hide:Relate:Transportistas)
  _Hide:Relate:Plantas.Kill
  DISPOSE(_Hide:Relate:Plantas)
  _Hide:Relate:Existencias.Kill
  DISPOSE(_Hide:Relate:Existencias)
  _Hide:Relate:Descargas.Kill
  DISPOSE(_Hide:Relate:Descargas)
  _Hide:Relate:Viajes.Kill
  DISPOSE(_Hide:Relate:Viajes)
  _Hide:Relate:Densidades_Corregidas.Kill
  DISPOSE(_Hide:Relate:Densidades_Corregidas)
  _Hide:Relate:Niveles_Volumenes.Kill
  DISPOSE(_Hide:Relate:Niveles_Volumenes)
  _Hide:Relate:t_tanques.Kill
  DISPOSE(_Hide:Relate:t_tanques)
  _Hide:Relate:Tanques_plantas.Kill
  DISPOSE(_Hide:Relate:Tanques_plantas)
  _Hide:Relate:Presiones_corregidas.Kill
  DISPOSE(_Hide:Relate:Presiones_corregidas)
  _Hide:Relate:Localidades_GLP.Kill
  DISPOSE(_Hide:Relate:Localidades_GLP)
  _Hide:Relate:Mediciones.Kill
  DISPOSE(_Hide:Relate:Mediciones)
  _Hide:Relate:aux_sql.Kill
  DISPOSE(_Hide:Relate:aux_sql)
  _Hide:Relate:programacion.Kill
  DISPOSE(_Hide:Relate:programacion)
  _Hide:Relate:Procedencias.Kill
  DISPOSE(_Hide:Relate:Procedencias)
  _Hide:Relate:Mediciones_aux.Kill
  DISPOSE(_Hide:Relate:Mediciones_aux)
  _Hide:Relate:parametros.Kill
  DISPOSE(_Hide:Relate:parametros)
  _Hide:Relate:PlantasStock.Kill
  DISPOSE(_Hide:Relate:PlantasStock)
  _Hide:Relate:viajes_anticipos.Kill
  DISPOSE(_Hide:Relate:viajes_anticipos)


Hide:Relate:Proveedores.Init PROCEDURE
  CODE
  _Hide:Access:Proveedores.Init
  SELF.Init(Access:Proveedores,1)


Hide:Access:Proveedores.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= pro:Record
  SELF.FileNameValue = 'Proveedores'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Proveedores
  PARENT.Init
  Access:Proveedores &= SELF


Hide:Access:Proveedores.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Proveedores &= NULL                               ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Proveedores.PrimeFields PROCEDURE

  CODE
  pro:importe_DNL = 0                                      ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Proveedores.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Proveedores,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(pro:PK_proveedor,'Id Proveedor',1)
    SELF.AddKey(pro:FK_PROVEEDOR_CONTABLE,'PROVEEDOR CONTABLE',0)
    SELF.AddKey(pro:K_proveedor,'Proveedor',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Proveedores.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Viajes,RI:CASCADE,RI:RESTRICT,via:FK_PROVEEDOR)
  SELF.AddRelationLink(pro:id_proveedor,via:id_proveedor)

  SELF.AddRelation(Relate:programacion,RI:CASCADE,RI:RESTRICT,prog:FK_PROVEEDOR)
  SELF.AddRelationLink(pro:id_proveedor,prog:id_proveedor)

  SELF.AddRelation(Relate:contactos_proveedores,RI:None,RI:None,cont:FK_PROVEEDOR)
  SELF.AddRelationLink(pro:id_proveedor,cont:id_proveedor)

  SELF.AddRelation(Relate:Proveedores_contable)


Hide:Relate:Proveedores.Kill PROCEDURE

  CODE
  _Hide:Access:Proveedores.Kill                            ! Kill the file manager
  PARENT.Kill
  Relate:Proveedores &= NULL                               ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Proveedores)                        ! destroy the file manager


Hide:Relate:Transportistas.Init PROCEDURE
  CODE
  _Hide:Access:Transportistas.Init
  SELF.Init(Access:Transportistas,1)


Hide:Access:Transportistas.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= tra:Record
  SELF.FileNameValue = 'Transportistas'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Transportistas
  PARENT.Init
  Access:Transportistas &= SELF


Hide:Access:Transportistas.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Transportistas &= NULL                            ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Transportistas.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Transportistas,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(tra:PK_TRANSPORTISTA,'Id Transportista',1)
    SELF.AddKey(tra:K_TRANSPORTISTA,'Transportista',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Transportistas.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Viajes,RI:CASCADE,RI:RESTRICT,via:FK_TRANSPORTISTA)
  SELF.AddRelationLink(tra:id_transportista,via:id_transportista)

  SELF.AddRelation(Relate:contactos_Transportista,RI:None,RI:None,cont1:FK_TRANSPORTISTA)
  SELF.AddRelationLink(tra:id_transportista,cont1:id_transportista)


Hide:Relate:Transportistas.Kill PROCEDURE

  CODE
  _Hide:Access:Transportistas.Kill                         ! Kill the file manager
  PARENT.Kill
  Relate:Transportistas &= NULL                            ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Transportistas)                     ! destroy the file manager


Hide:Relate:Plantas.Init PROCEDURE
  CODE
  _Hide:Access:Plantas.Init
  SELF.Init(Access:Plantas,1)


Hide:Access:Plantas.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= pla:Record
  SELF.FileNameValue = 'Plantas'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Plantas
  PARENT.Init
  Access:Plantas &= SELF


Hide:Access:Plantas.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Plantas &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Plantas.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Plantas,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(pla:PK__plantas__7D439ABD,'Id Planta',1)
    SELF.AddKey(pla:PLA_PLA_FK_LOCALIDAD,'Localidad',0)
    SELF.AddKey(pla:K_LOCALIDAD_PLANTA,'Localidad',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Plantas.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Existencias,RI:CASCADE,RI:RESTRICT,exi:EXI_FK_PLANTA)
  SELF.AddRelationLink(pla:ID_PLANTA,exi:id_planta)

  SELF.AddRelation(Relate:Tanques_plantas,RI:CASCADE,RI:RESTRICT,tan:FK_PLANTA)
  SELF.AddRelationLink(pla:ID_PLANTA,tan:id_planta)

  SELF.AddRelation(Relate:Localidades_GLP)

  SELF.AddRelation(Relate:Mediciones,RI:CASCADE,RI:RESTRICT,med:FK_PLANTA)
  SELF.AddRelationLink(pla:ID_PLANTA,med:id_planta)

  SELF.AddRelation(Relate:PlantasStock,RI:None,RI:None,STK:FK_PLANTA)
  SELF.AddRelationLink(pla:ID_PLANTA,STK:id_planta)

  SELF.AddRelation(Relate:Descargas,RI:None,RI:None,des:FK_PLANTA)
  SELF.AddRelationLink(pla:ID_PLANTA,des:id_planta)

  SELF.AddRelation(Relate:ExistenciasAlias1,RI:None,RI:None,exi1:EXI_FK_PLANTA)
  SELF.AddRelationLink(pla:ID_PLANTA,exi1:id_planta)


Hide:Relate:Plantas.Kill PROCEDURE

  CODE
  _Hide:Access:Plantas.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:Plantas &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Plantas)                            ! destroy the file manager


Hide:Relate:Existencias.Init PROCEDURE
  CODE
  _Hide:Access:Existencias.Init
  SELF.Init(Access:Existencias,1)


Hide:Access:Existencias.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= exi:Record
  SELF.FileNameValue = 'Existencias'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Existencias
  PARENT.Init
  Access:Existencias &= SELF


Hide:Access:Existencias.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Existencias &= NULL                               ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Existencias.PrimeFields PROCEDURE

  CODE
  exi:utilizada = 0                                        ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Existencias.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Existencias,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(exi:KEY__WA_Sys_ID_STOCK_32AB8735,'STOCK',0)
    SELF.AddKey(exi:FK_EXISTENCIA_ANTERIOR,'exi:FK_EXISTENCIA_ANTERIOR',0)
    SELF.AddKey(exi:PK__EXISTENC__36B12243,'Id',1)
    SELF.AddKey(exi:EXI_K_PLANTA_Y_ID_EXISTENCIA,'Planta',0)
    SELF.AddKey(exi:EXI_FK_PLANTA,'Planta',0)
    SELF.AddKey(exi:FK_LOCALIDAD,'LOCALIDAD',0)
    SELF.AddKey(exi:K_LOCALIDAD_EXISTENCIAS,'exi:K_LOCALIDAD_EXISTENCIAS',0)
    SELF.AddKey(exi:K_LOCALIDAD_PLANTA_EXISTENCIA,'exi:K_LOCALIDAD_PLANTA_EXISTENCIA',0)
    SELF.AddKey(exi:EXI_EXI_FK_PLANTA,'exi:EXI_EXI_FK_PLANTA',0)
    SELF.AddKey(exi:KEY__WA_Sys_id_localidad_32AB8735,'exi:KEY__WA_Sys_id_localidad_32AB8735',0)
    SELF.AddKey(exi:KEY__WA_Sys_EXISTENCIA_32AB8735,'exi:KEY__WA_Sys_EXISTENCIA_32AB8735',0)
    SELF.AddKey(exi:KEY__WA_Sys_EXISTENCIA_ANTERIOR_32AB8735,'exi:KEY__WA_Sys_EXISTENCIA_ANTERIOR_32AB8735',0)
    SELF.AddKey(exi:KEY__WA_Sys_PORC_EXISTENCIA_32AB8735,'exi:KEY__WA_Sys_PORC_EXISTENCIA_32AB8735',0)
    SELF.AddKey(exi:KEY__WA_Sys_CONSUMO_32AB8735,'exi:KEY__WA_Sys_CONSUMO_32AB8735',0)
    SELF.AddKey(exi:KEY__WA_Sys_CAPACIDAD_PLANTA_32AB8735,'exi:KEY__WA_Sys_CAPACIDAD_PLANTA_32AB8735',0)
    SELF.AddKey(exi:KEY__WA_Sys_FECHA_LECTURA_32AB8735,'exi:KEY__WA_Sys_FECHA_LECTURA_32AB8735',0)
    SELF.AddKey(exi:KEY__WA_Sys_AUTONOMIA_32AB8735,'exi:KEY__WA_Sys_AUTONOMIA_32AB8735',0)
    SELF.AddKey(exi:KEY__WA_Sys_ultima_descarga_32AB8735,'exi:KEY__WA_Sys_ultima_descarga_32AB8735',0)
    SELF.AddKey(exi:K_FECHA_LOCALIDAD_PLANTA,'exi:K_FECHA_LOCALIDAD_PLANTA',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Existencias.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Plantas)

  SELF.AddRelation(Relate:Mediciones,RI:None,RI:None,med:FK_EXISTENCIA)
  SELF.AddRelationLink(exi:id_existencia,med:id_existencia)

  SELF.AddRelation(Relate:Localidades_GLP)

  SELF.AddRelation(Relate:PlantasStock)

  SELF.AddRelation(Relate:ExistenciasAlias1)

  SELF.AddRelation(Relate:Descargas)


Hide:Relate:Existencias.Kill PROCEDURE

  CODE
  _Hide:Access:Existencias.Kill                            ! Kill the file manager
  PARENT.Kill
  Relate:Existencias &= NULL                               ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Existencias)                        ! destroy the file manager


Hide:Relate:Descargas.Init PROCEDURE
  CODE
  _Hide:Access:Descargas.Init
  SELF.Init(Access:Descargas,1)


Hide:Access:Descargas.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= des:Record
  SELF.FileNameValue = 'Descargas'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Descargas
  PARENT.Init
  Access:Descargas &= SELF


Hide:Access:Descargas.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Descargas &= NULL                                 ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Descargas.PrimeFields PROCEDURE

  CODE
  des:descargado = 0                                       ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Descargas.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Descargas,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(des:K_planta,'Planta',0)
    SELF.AddKey(des:FK_LOCALIDAD,'LOCALIDAD',0)
    SELF.AddKey(des:K_LOCALIDAD_DESCARGA,'des:K_LOCALIDAD_DESCARGA',0)
    SELF.AddKey(des:PK_descargas,'Id Descarga',1)
    SELF.AddKey(des:KEY__WA_Sys_id_viaje_3D5E1FD2,'Viajes',0)
    SELF.AddKey(des:FK_PLANTA,'Plantas',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Descargas.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Viajes)

  SELF.AddRelation(Relate:Localidades_GLP)

  SELF.AddRelation(Relate:Plantas)

  SELF.AddRelation(Relate:Existencias,RI:None,RI:None,exi:FK_EXISTENCIA_ANTERIOR)
  SELF.AddRelationLink(des:id_descarga,exi:id_existencia_anterior)


Hide:Relate:Descargas.Kill PROCEDURE

  CODE
  _Hide:Access:Descargas.Kill                              ! Kill the file manager
  PARENT.Kill
  Relate:Descargas &= NULL                                 ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Descargas)                          ! destroy the file manager


Hide:Relate:Viajes.Init PROCEDURE
  CODE
  _Hide:Access:Viajes.Init
  SELF.Init(Access:Viajes,1)


Hide:Access:Viajes.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= via:Record
  SELF.FileNameValue = 'Viajes'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Viajes
  PARENT.Init
  Access:Viajes &= SELF


Hide:Access:Viajes.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Viajes &= NULL                                    ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Viajes.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Viajes,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(via:VIA_FK_PROGRAMACION,'PROGRAMACION',0)
    SELF.AddKey(via:VIA_FK_PROCEDENCIA,'PROCEDENCIA',0)
    SELF.AddKey(via:VIA_K_PROVEEDOR_ESTADO,'via:VIA_K_PROVEEDOR_ESTADO',0)
    SELF.AddKey(via:K_ano_mes,'via:K_ano_mes',0)
    SELF.AddKey(via:K_PROCEDENCIA,'via:K_PROCEDENCIA',0)
    SELF.AddKey(via:PK_viajes,'Id Viaje',1)
    SELF.AddKey(via:K_ESTADO,'ESTADO',0)
    SELF.AddKey(via:FK_TRANSPORTISTA,'Transportista',0)
    SELF.AddKey(via:FK_PROVEEDOR,'Proveedor',0)
    SELF.AddKey(via:K_REMITO,'NRO REMITO',0)
    SELF.AddKey(via:K_PROVEEDOR_ID_VIAJE,'Proveedor',0)
    SELF.AddKey(via:K_ID_TRANSPORTISTA_VIAJE,'transportista',0)
    SELF.AddKey(via:FK_LOCALIDAD,'LOCALIDAD',0)
    SELF.AddKey(via:FK_SOLICITUD,'via:FK_SOLICITUD',0)
    SELF.AddKey(via:FK_FACTURA,'via:FK_FACTURA',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Viajes.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Descargas,RI:CASCADE,RI:RESTRICT,des:KEY__WA_Sys_id_viaje_3D5E1FD2)
  SELF.AddRelationLink(via:id_viaje,des:id_viaje)

  SELF.AddRelation(Relate:Transportistas)

  SELF.AddRelation(Relate:Proveedores)

  SELF.AddRelation(Relate:Procedencias)

  SELF.AddRelation(Relate:Localidades_GLP)

  SELF.AddRelation(Relate:programacion)

  SELF.AddRelation(Relate:viajes_anticipos)

  SELF.AddRelation(Relate:Localidades_GLPAlias1)

  SELF.AddRelation(Relate:Despacho_viajes,RI:None,RI:None,Des3:FK_VIAJE)
  SELF.AddRelationLink(via:id_viaje,Des3:id_viaje)


Hide:Relate:Viajes.Kill PROCEDURE

  CODE
  _Hide:Access:Viajes.Kill                                 ! Kill the file manager
  PARENT.Kill
  Relate:Viajes &= NULL                                    ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Viajes)                             ! destroy the file manager


Hide:Relate:Densidades_Corregidas.Init PROCEDURE
  CODE
  _Hide:Access:Densidades_Corregidas.Init
  SELF.Init(Access:Densidades_Corregidas,1)


Hide:Access:Densidades_Corregidas.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= den:Record
  SELF.FileNameValue = 'Densidades_Corregidas'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Densidades_Corregidas
  PARENT.Init
  Access:Densidades_Corregidas &= SELF


Hide:Access:Densidades_Corregidas.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Densidades_Corregidas &= NULL                     ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Densidades_Corregidas.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Densidades_Corregidas,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(den:PK_tabla_densidad_corregida,'Id',1)
    SELF.AddKey(den:K_temp_Den,'Temp y Dens',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Densidades_Corregidas.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Mediciones,RI:CASCADE,RI:RESTRICT,med:FK_DENSIDAD)
  SELF.AddRelationLink(den:id_factor,med:id_factor_densidad)


Hide:Relate:Densidades_Corregidas.Kill PROCEDURE

  CODE
  _Hide:Access:Densidades_Corregidas.Kill                  ! Kill the file manager
  PARENT.Kill
  Relate:Densidades_Corregidas &= NULL                     ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Densidades_Corregidas)              ! destroy the file manager


Hide:Relate:Niveles_Volumenes.Init PROCEDURE
  CODE
  _Hide:Access:Niveles_Volumenes.Init
  SELF.Init(Access:Niveles_Volumenes,1)


Hide:Access:Niveles_Volumenes.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= niv:Record
  SELF.FileNameValue = 'Niveles_Volumenes'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Niveles_Volumenes
  PARENT.Init
  Access:Niveles_Volumenes &= SELF


Hide:Access:Niveles_Volumenes.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Niveles_Volumenes &= NULL                         ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Niveles_Volumenes.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Niveles_Volumenes,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(niv:PK_niveles_volumenes,'niv:PK_niveles_volumenes',1)
    SELF.AddKey(niv:UK_TANQUE_NIVEL,'Tanque y Nivel',0)
    SELF.AddKey(niv:FK_TANQUE,'Tipo de tanque',0)
    SELF.AddKey(niv:K_NIVEL_REGLA,'niv:K_NIVEL_REGLA',0)
    SELF.AddKey(niv:K_TANQUE,'TIPO TANQUE',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Niveles_Volumenes.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:t_tanques)

  SELF.AddRelation(Relate:Mediciones,RI:None,RI:None,med:FK_NIVEL)
  SELF.AddRelationLink(niv:id_nivel,med:id_nivel)


Hide:Relate:Niveles_Volumenes.Kill PROCEDURE

  CODE
  _Hide:Access:Niveles_Volumenes.Kill                      ! Kill the file manager
  PARENT.Kill
  Relate:Niveles_Volumenes &= NULL                         ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Niveles_Volumenes)                  ! destroy the file manager


Hide:Relate:t_tanques.Init PROCEDURE
  CODE
  _Hide:Access:t_tanques.Init
  SELF.Init(Access:t_tanques,1)


Hide:Access:t_tanques.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= t_t:Record
  SELF.FileNameValue = 't_tanques'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= t_tanques
  PARENT.Init
  Access:t_tanques &= SELF


Hide:Access:t_tanques.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:t_tanques &= NULL                                 ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:t_tanques.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(t_tanques,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(t_t:PK_t_tanques,'Id Tanque',1)
    SELF.AddKey(t_t:K_MODELO,'Modelo',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:t_tanques.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Niveles_Volumenes,RI:CASCADE,RI:RESTRICT,niv:K_TANQUE)
  SELF.AddRelationLink(t_t:idt_tanque,niv:idt_tanque)

  SELF.AddRelation(Relate:Tanques_plantas,RI:None,RI:None,tan:FK_TANQUE)
  SELF.AddRelationLink(t_t:idt_tanque,tan:idt_tanques)


Hide:Relate:t_tanques.Kill PROCEDURE

  CODE
  _Hide:Access:t_tanques.Kill                              ! Kill the file manager
  PARENT.Kill
  Relate:t_tanques &= NULL                                 ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:t_tanques)                          ! destroy the file manager


Hide:Relate:Tanques_plantas.Init PROCEDURE
  CODE
  _Hide:Access:Tanques_plantas.Init
  SELF.Init(Access:Tanques_plantas,1)


Hide:Access:Tanques_plantas.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= tan:Record
  SELF.FileNameValue = 'Tanques_plantas'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Tanques_plantas
  PARENT.Init
  Access:Tanques_plantas &= SELF


Hide:Access:Tanques_plantas.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Tanques_plantas &= NULL                           ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Tanques_plantas.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Tanques_plantas,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(tan:PK_tanques,'Tanque',1)
    SELF.AddKey(tan:FK_PLANTA,'Planta',0)
    SELF.AddKey(tan:FK_TANQUE,'TIPO TANQUE',0)
    SELF.AddKey(tan:k_planta,'Planta',0)
    SELF.AddKey(tan:K_TANQUE,'Tipo de tanque',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Tanques_plantas.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Plantas)

  SELF.AddRelation(Relate:Mediciones,RI:CASCADE,RI:RESTRICT,med:FK_TANQUE)
  SELF.AddRelationLink(tan:id_tanque,med:id_tanque)

  SELF.AddRelation(Relate:Mediciones_aux,RI:CASCADE,RI:RESTRICT,med1:FK_TANQUE)
  SELF.AddRelationLink(tan:id_tanque,med1:id_tanque)

  SELF.AddRelation(Relate:t_tanques)


Hide:Relate:Tanques_plantas.Kill PROCEDURE

  CODE
  _Hide:Access:Tanques_plantas.Kill                        ! Kill the file manager
  PARENT.Kill
  Relate:Tanques_plantas &= NULL                           ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Tanques_plantas)                    ! destroy the file manager


Hide:Relate:Presiones_corregidas.Init PROCEDURE
  CODE
  _Hide:Access:Presiones_corregidas.Init
  SELF.Init(Access:Presiones_corregidas,1)


Hide:Access:Presiones_corregidas.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= pre:Record
  SELF.FileNameValue = 'Presiones_corregidas'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Presiones_corregidas
  PARENT.Init
  Access:Presiones_corregidas &= SELF


Hide:Access:Presiones_corregidas.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Presiones_corregidas &= NULL                      ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Presiones_corregidas.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Presiones_corregidas,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(pre:PK_factor_presion,'Id',1)
    SELF.AddKey(pre:K_presion_temp,'Presion Temp',0)
    SELF.AddKey(pre:k_presion,'pre:k_presion',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Presiones_corregidas.Kill PROCEDURE

  CODE
  _Hide:Access:Presiones_corregidas.Kill                   ! Kill the file manager
  PARENT.Kill
  Relate:Presiones_corregidas &= NULL                      ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Presiones_corregidas)               ! destroy the file manager


Hide:Relate:Localidades_GLP.Init PROCEDURE
  CODE
  _Hide:Access:Localidades_GLP.Init
  SELF.Init(Access:Localidades_GLP,1)


Hide:Access:Localidades_GLP.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Loc:Record
  SELF.FileNameValue = 'Localidades_GLP'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Localidades_GLP
  PARENT.Init
  Access:Localidades_GLP &= SELF


Hide:Access:Localidades_GLP.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Localidades_GLP &= NULL                           ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Localidades_GLP.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Localidades_GLP,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Loc:PK_localidad,'ID localidad',0)
    SELF.AddKey(Loc:K_localidad,'Localidad',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Localidades_GLP.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Plantas,RI:CASCADE,RI:RESTRICT,pla:PLA_PLA_FK_LOCALIDAD)
  SELF.AddRelationLink(Loc:id_localidad,pla:ID_LOCALIDAD)

  SELF.AddRelation(Relate:Viajes,RI:CASCADE,RI:RESTRICT,via:FK_LOCALIDAD)
  SELF.AddRelationLink(Loc:id_localidad,via:id_localidad)

  SELF.AddRelation(Relate:Existencias,RI:None,RI:None,exi:FK_LOCALIDAD)
  SELF.AddRelationLink(Loc:id_localidad,exi:id_localidad)

  SELF.AddRelation(Relate:PlantasStock,RI:None,RI:None,STK:FK_LOCALIDAD)
  SELF.AddRelationLink(Loc:id_localidad,STK:id_localidad)

  SELF.AddRelation(Relate:Descargas,RI:None,RI:None,des:FK_LOCALIDAD)
  SELF.AddRelationLink(Loc:id_localidad,des:id_localidad)

  SELF.AddRelation(Relate:ExistenciasAlias1,RI:None,RI:None,exi1:FK_LOCALIDAD)
  SELF.AddRelationLink(Loc:id_localidad,exi1:id_localidad)

  SELF.AddRelation(Relate:Mediciones,RI:None,RI:None,med:FK_LOCALIDAD)
  SELF.AddRelationLink(Loc:id_localidad,med:id_localidad)


Hide:Relate:Localidades_GLP.Kill PROCEDURE

  CODE
  _Hide:Access:Localidades_GLP.Kill                        ! Kill the file manager
  PARENT.Kill
  Relate:Localidades_GLP &= NULL                           ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Localidades_GLP)                    ! destroy the file manager


Hide:Relate:Mediciones.Init PROCEDURE
  CODE
  _Hide:Access:Mediciones.Init
  SELF.Init(Access:Mediciones,1)


Hide:Access:Mediciones.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= med:Record
  SELF.FileNameValue = 'Mediciones'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Mediciones
  PARENT.Init
  Access:Mediciones &= SELF


Hide:Access:Mediciones.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Mediciones &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Mediciones.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Mediciones,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(med:PK_mediciones,'med:PK_mediciones',1)
    SELF.AddKey(med:FK_EXISTENCIA,'EXISTENCIA',0)
    SELF.AddKey(med:FK_PLANTA,'PLANTA',0)
    SELF.AddKey(med:FK_DENSIDAD,'Factor densidad',0)
    SELF.AddKey(med:K_TANQUE,'Tanque',0)
    SELF.AddKey(med:K_MEDICIONES_EXISTENCIA,'med:K_MEDICIONES_EXISTENCIA',0)
    SELF.AddKey(med:FK_TANQUE,'med:FK_TANQUE',0)
    SELF.AddKey(med:FK_LOCALIDAD,'LOCALIDAD',0)
    SELF.AddKey(med:FK_NIVEL,'NIVEL',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Mediciones.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Plantas)

  SELF.AddRelation(Relate:Existencias)

  SELF.AddRelation(Relate:Tanques_plantas)

  SELF.AddRelation(Relate:Densidades_Corregidas)

  SELF.AddRelation(Relate:Localidades_GLP)

  SELF.AddRelation(Relate:Niveles_Volumenes)


Hide:Relate:Mediciones.Kill PROCEDURE

  CODE
  _Hide:Access:Mediciones.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:Mediciones &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Mediciones)                         ! destroy the file manager


Hide:Relate:aux_sql.Init PROCEDURE
  CODE
  _Hide:Access:aux_sql.Init
  SELF.Init(Access:aux_sql,1)


Hide:Access:aux_sql.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= aux:Record
  SELF.FileNameValue = 'aux_sql'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= aux_sql
  PARENT.Init
  Access:aux_sql &= SELF


Hide:Access:aux_sql.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:aux_sql &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:aux_sql.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(aux_sql,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:aux_sql.Kill PROCEDURE

  CODE
  _Hide:Access:aux_sql.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:aux_sql &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:aux_sql)                            ! destroy the file manager


Hide:Relate:programacion.Init PROCEDURE
  CODE
  _Hide:Access:programacion.Init
  SELF.Init(Access:programacion,1)


Hide:Access:programacion.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= prog:Record
  SELF.FileNameValue = 'programacion'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= programacion
  PARENT.Init
  Access:programacion &= SELF


Hide:Access:programacion.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:programacion &= NULL                              ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:programacion.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(programacion,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(prog:K_ANO_MES_SEMANA,'prog:K_ANO_MES_SEMANA',0)
    SELF.AddKey(prog:PK_PROGRAMACION,'Programación',1)
    SELF.AddKey(prog:FK_PROVEEDOR,'Id Proveedor',0)
    SELF.AddKey(prog:K_ANO_MES_SEMAMA_PROVEEDOR,'prog:K_ANO_MES_SEMAMA_PROVEEDOR',0)
    SELF.AddKey(prog:K_proveedor,'Proveedor',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:programacion.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Proveedores)

  SELF.AddRelation(Relate:Viajes,RI:CASCADE,RI:RESTRICT,via:VIA_FK_PROGRAMACION)
  SELF.AddRelationLink(prog:id_programacion,via:id_programacion)


Hide:Relate:programacion.Kill PROCEDURE

  CODE
  _Hide:Access:programacion.Kill                           ! Kill the file manager
  PARENT.Kill
  Relate:programacion &= NULL                              ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:programacion)                       ! destroy the file manager


Hide:Relate:Procedencias.Init PROCEDURE
  CODE
  _Hide:Access:Procedencias.Init
  SELF.Init(Access:Procedencias,1)


Hide:Access:Procedencias.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= pro1:Record
  SELF.FileNameValue = 'Procedencias'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Procedencias
  PARENT.Init
  Access:Procedencias &= SELF


Hide:Access:Procedencias.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Procedencias &= NULL                              ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Procedencias.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Procedencias,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(pro1:PK_PROCEDENCIA,'Id Procedencia',1)
    SELF.AddKey(pro1:K_procedencia,'pro1:K_procedencia',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Procedencias.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Viajes,RI:CASCADE,RI:RESTRICT,via:VIA_FK_PROCEDENCIA)
  SELF.AddRelationLink(pro1:id_procedencia,via:id_procedencia)

  SELF.AddRelation(Relate:Despacho_aduana,RI:None,RI:None,des1:FK_PROCEDENCIA)
  SELF.AddRelationLink(pro1:id_procedencia,des1:id_procedencia)


Hide:Relate:Procedencias.Kill PROCEDURE

  CODE
  _Hide:Access:Procedencias.Kill                           ! Kill the file manager
  PARENT.Kill
  Relate:Procedencias &= NULL                              ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Procedencias)                       ! destroy the file manager


Hide:Relate:Mediciones_aux.Init PROCEDURE
  CODE
  _Hide:Access:Mediciones_aux.Init
  SELF.Init(Access:Mediciones_aux,1)


Hide:Access:Mediciones_aux.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= med1:Record
  SELF.FileNameValue = 'Mediciones_aux'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Mediciones_aux
  PARENT.Init
  Access:Mediciones_aux &= SELF


Hide:Access:Mediciones_aux.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Mediciones_aux &= NULL                            ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Mediciones_aux.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Mediciones_aux,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(med1:PK_mediciones,'med1:PK_mediciones',1)
    SELF.AddKey(med1:FK_EXISTENCIA,'EXISTENCIA',0)
    SELF.AddKey(med1:FK_PLANTA,'PLANTA',0)
    SELF.AddKey(med1:FK_DENSIDAD,'Factor densidad',0)
    SELF.AddKey(med1:K_TANQUE,'Tanque',0)
    SELF.AddKey(med1:FK_TANQUE,'med1:FK_TANQUE',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Mediciones_aux.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Tanques_plantas)


Hide:Relate:Mediciones_aux.Kill PROCEDURE

  CODE
  _Hide:Access:Mediciones_aux.Kill                         ! Kill the file manager
  PARENT.Kill
  Relate:Mediciones_aux &= NULL                            ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Mediciones_aux)                     ! destroy the file manager


Hide:Relate:parametros.Init PROCEDURE
  CODE
  _Hide:Access:parametros.Init
  SELF.Init(Access:parametros,1)


Hide:Access:parametros.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= par:Record
  SELF.FileNameValue = 'parametros'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= parametros
  PARENT.Init
  Access:parametros &= SELF


Hide:Access:parametros.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:parametros &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:parametros.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(parametros,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(par:PK_parametros,'par:PK_parametros',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:parametros.Kill PROCEDURE

  CODE
  _Hide:Access:parametros.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:parametros &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:parametros)                         ! destroy the file manager


Hide:Relate:PlantasStock.Init PROCEDURE
  CODE
  _Hide:Access:PlantasStock.Init
  SELF.Init(Access:PlantasStock,1)


Hide:Access:PlantasStock.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= STK:Record
  SELF.FileNameValue = 'PlantasStock'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= PlantasStock
  PARENT.Init
  Access:PlantasStock &= SELF


Hide:Access:PlantasStock.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:PlantasStock &= NULL                              ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:PlantasStock.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(PlantasStock,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(STK:FK_PLANTA,'STK:FK_PLANTA',0)
    SELF.AddKey(STK:FK_LOCALIDAD,'LOCALIDAD',0)
    SELF.AddKey(STK:K_PLANTA_STOCK,'STK:K_PLANTA_STOCK',0)
    SELF.AddKey(STK:K_LOCALIDAD_STOCK,'STK:K_LOCALIDAD_STOCK',0)
    SELF.AddKey(STK:PK_PlantasStock,'STK:PK_PlantasStock',1)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:PlantasStock.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Plantas)

  SELF.AddRelation(Relate:Localidades_GLP)

  SELF.AddRelation(Relate:Existencias,RI:None,RI:None,exi:KEY__WA_Sys_ID_STOCK_32AB8735)
  SELF.AddRelationLink(STK:id_stock,exi:id_stock)


Hide:Relate:PlantasStock.Kill PROCEDURE

  CODE
  _Hide:Access:PlantasStock.Kill                           ! Kill the file manager
  PARENT.Kill
  Relate:PlantasStock &= NULL                              ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:PlantasStock)                       ! destroy the file manager


Hide:Relate:viajes_anticipos.Init PROCEDURE
  CODE
  _Hide:Access:viajes_anticipos.Init
  SELF.Init(Access:viajes_anticipos,1)


Hide:Access:viajes_anticipos.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= sol:Record
  SELF.FileNameValue = 'viajes_anticipos'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= viajes_anticipos
  PARENT.Init
  Access:viajes_anticipos &= SELF


Hide:Access:viajes_anticipos.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:viajes_anticipos &= NULL                          ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:viajes_anticipos.PrimeFields PROCEDURE

  CODE
  sol:importe = 0                                          ! Assign initial field value
  sol:producto = 0                                         ! Assign initial field value
  sol:DIAS_DNL = 0                                         ! Assign initial field value
  sol:IMPORTE_DNL = 0                                      ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:viajes_anticipos.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(viajes_anticipos,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(sol:PK_solicitudes_anticipos,'sol:PK_solicitudes_anticipos',1)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:viajes_anticipos.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Viajes,RI:None,RI:None,via:FK_SOLICITUD)
  SELF.AddRelationLink(sol:id_solicitud,via:id_solicitud)


Hide:Relate:viajes_anticipos.Kill PROCEDURE

  CODE
  _Hide:Access:viajes_anticipos.Kill                       ! Kill the file manager
  PARENT.Kill
  Relate:viajes_anticipos &= NULL                          ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:viajes_anticipos)                   ! destroy the file manager

