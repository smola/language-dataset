supplier "translateScriptNameSupplyTransporter"
{
    enum lights
    {
        "translateCommandStateLightsAUTO",
        "translateCommandStateLightsON",
        "translateCommandStateLightsOFF",
multi:
        "translateCommandStateLightsMode"
    }
    
    enum traceMode
    {
        "translateCommandStateTraceOFF",
        "translateCommandStateTraceON",
multi:
        "translateCommandStateTraceMode"
    }
    int m_nMoveToX;
    int m_nMoveToY;
    int m_nMoveToZ;
    int  m_nLandCounter;
    
    state Initialize;
    state Nothing;
    state StartMoving;
    state Moving;
    state StartLanding;
    state Landing;
    state MovingToAssemblyPoint;
    state MovingToSupplyCenter;
    state MovingToObjectForSupply;
    state LoadingAmmo;
    state PuttingAmmo;
    
    function int Land()
    {
        if (!IsOnGround())
        {
            m_nMoveToX = GetLocationX();
            m_nMoveToY = GetLocationY();
            m_nMoveToZ = GetLocationZ();
            if (!IsFreePoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ))
            {
                if (Rand(2))
                {
                    m_nMoveToX = m_nMoveToX + (Rand(m_nLandCounter) + 1);
                }
                else
                {
                    m_nMoveToX = m_nMoveToX - (Rand(m_nLandCounter) + 1);
                }
                if (Rand(2))
                {
                    m_nMoveToY = m_nMoveToY + (Rand(m_nLandCounter) + 1);
                }
                else
                {
                    m_nMoveToY = m_nMoveToY - (Rand(m_nLandCounter) + 1);
                }
                m_nLandCounter = m_nLandCounter + 1;
                if (IsFreePoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ))
                {
                    CallMoveAndLandToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                }
                else
                {
                    CallMoveLowToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                }
            }
            else
            {
                CallMoveAndLandToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
            }
            return true;
        }
        else
        {
            return false;
        }
    }
    //-------------------------------------------------------
    state Initialize
    {
        return Nothing;
    }
    
    state Nothing
    {
        int bIsEnd;
        if (HaveObjectsForSupply())
        {
            //kontynujemy zaopatrzenie bo nie mozna zostawic zadnego obiektu
            bIsEnd = false;
            while (!bIsEnd && !CanCurrentObjectBeSupplied())
            {
                if (!NextObjectForSupply())
                {
                    bIsEnd = true;
                }
            }
            if (!bIsEnd)
            {
                m_nMoveToX = GetCurrentPutSupplyPositionX();
                m_nMoveToY = GetCurrentPutSupplyPositionY();
                m_nMoveToZ = GetCurrentPutSupplyPositionZ();
                CallMoveToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                return MovingToObjectForSupply;
            }
            else
            {
                return Nothing;
            }
        }
        else
        {
            return Nothing;
        }
    }
    
    state StartMoving
    {
        return Moving, 20;
    }
    //--------------------------------------------------------------------------
    state Moving
    {
        if(traceMode)TraceD("state Moving                                                \n");
        if (IsMoving())
        {
            return Moving;
        }
        else
        {
            NextCommand(1);
            return Nothing;
        }
    }
    state StartLanding
    {
        return Landing, 20;
    }
    //--------------------------------------------------------------------------
    state Landing
    {
        if (IsMoving())
        {
            if ((GetLocationX() == m_nMoveToX) && (GetLocationY() == m_nMoveToY) && (GetLocationZ() == m_nMoveToZ) && 
                !IsFreePoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ))
            {
                if (!Land())
                {
                    NextCommand(1);
                    return Nothing;
                }
            }
            return Landing;
        }
        else
        {
            if (!Land())
            {
                NextCommand(1);
                return Nothing;
            }
            return Landing;
        }
    }
    //--------------------------------------------------------------------------
    
    state MovingToAssemblyPoint
    {
        if(traceMode)TraceD("state MovingToAssemblyPoint                                \n");
        if (IsMoving())
        {
            return MovingToAssemblyPoint;
        }
        else
        {
            return Nothing;
        }
    }
    
    state MovingToSupplyCenter
    {
        int nPosX;
        int nPosY;
        int nPosZ;
        if (IsMoving())
        {
            if(traceMode)TraceD("M -> SC                                                \n");
                        nPosX = GetLocationX();
            nPosY = GetLocationY();
            nPosZ = GetLocationZ();
            if ((nPosX == m_nMoveToX) && (nPosY == m_nMoveToY) && (nPosZ == m_nMoveToZ))//added 25.01.2000
                            CallStopMoving();
            return MovingToSupplyCenter;
        }
        else
        {
            nPosX = GetLocationX();
            nPosY = GetLocationY();
            nPosZ = GetLocationZ();
            if(traceMode)TraceD("M -> SC                                                \n");
            if ((nPosX == m_nMoveToX) && (nPosY == m_nMoveToY) && (nPosZ == m_nMoveToZ))
            {
                if(traceMode)TraceD("M -> SC OK                                               \n");
                CallLoadAmmo();
                return LoadingAmmo;
            }
            else
            {
                if(traceMode)TraceD("M -> SC Again                                                \n");
                //CallMoveAndLandToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                                CallMoveToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);//dodane 09.03.2000
                return MovingToSupplyCenter;
            }
        }
    }
    
    state MovingToObjectForSupply
    {
        int nPosX;
        int nPosY;
        int nPosZ;
        int bIsEnd;
        if (IsMoving())
        {
            if (!CanCurrentObjectBeSupplied())
            {
                CallStopMoving();
                return MovingToObjectForSupply;
            }
            CheckCurrentPutSupplyLocation();
            if(m_nMoveToX != GetCurrentPutSupplyPositionX()||
                m_nMoveToY != GetCurrentPutSupplyPositionY())
            {
                m_nMoveToX = GetCurrentPutSupplyPositionX();
                m_nMoveToY = GetCurrentPutSupplyPositionY();
                m_nMoveToZ = GetCurrentPutSupplyPositionZ();
                CallMoveToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
            }       
            
            //XXXMD zrobic gonienie czolgu
            if(traceMode)TraceD("state MovingToObjectForSupply 1                               \n");
            return MovingToObjectForSupply;
        }
        else
        {
            if (IsInGoodCurrentPutSupplyLocation())
            {
                if(traceMode)TraceD("state MovingToObjectForSupply 2                               \n");
                CallPutAmmo();
                return PuttingAmmo;
            }
            else
            {
                if(traceMode)TraceD("state MovingToObjectForSupply 3                               \n");
                //nie jest w dobrym miejscu
                if (CanCurrentObjectBeSupplied())
                {
                    m_nMoveToX = GetCurrentPutSupplyPositionX();
                    m_nMoveToY = GetCurrentPutSupplyPositionY();
                    m_nMoveToZ = GetCurrentPutSupplyPositionZ();
                    if(traceMode)TraceD("state MovingToObjectForSupply 4                               \n");
                    CallMoveToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                    return MovingToObjectForSupply;
                }
                else
                {
                    if(traceMode)TraceD("state MovingToObjectForSupply - Can't be supplied\n");
                    //nie mozna do niego dojechac (pod ziemia), lub zabity - biezemy nastepnego
                    bIsEnd = false;
                    do
                    {
                        if (!NextObjectForSupply())
                        {
                            bIsEnd = true;
                        }
                    }
                    while (!bIsEnd && !CanCurrentObjectBeSupplied());
                    if (!bIsEnd)
                    {
                        m_nMoveToX = GetCurrentPutSupplyPositionX();
                        m_nMoveToY = GetCurrentPutSupplyPositionY();
                        m_nMoveToZ = GetCurrentPutSupplyPositionZ();
                        CallMoveToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                        return MovingToObjectForSupply;
                    }
                    else
                    {
                        //nie ma juz obiektow do zaopatrzenia - wracamy do punktu zbiorki
                        if (GetSupplyCenterBuilding() != null)
                        {
                            m_nMoveToX = GetSupplyCenterAssemblyPositionX();
                            m_nMoveToY = GetSupplyCenterAssemblyPositionY();
                            m_nMoveToZ = GetSupplyCenterAssemblyPositionZ();
                            CallMoveToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                            return MovingToAssemblyPoint;
                        }
                        else
                        {
                            return Nothing;
                        }
                    }
                }
            }
        }
    }
    
    state LoadingAmmo
    {
        int bIsEnd;
        if (IsLoadingAmmo())
        {
            return LoadingAmmo;
        }
        else
        {
            if (HaveObjectsForSupply())
            {
                bIsEnd = false;
                while (!bIsEnd && !CanCurrentObjectBeSupplied())
                {
                    if (!NextObjectForSupply())
                    {
                        bIsEnd = true;
                    }
                }
                if (!bIsEnd)
                {
                    m_nMoveToX = GetCurrentPutSupplyPositionX();
                    m_nMoveToY = GetCurrentPutSupplyPositionY();
                    m_nMoveToZ = GetCurrentPutSupplyPositionZ();
                    CallMoveToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                    return MovingToObjectForSupply;
                }
                else
                {
                    //nie ma obiektow do zaopatrzenia - wracamy do punktu zbiorki
                    if (GetSupplyCenterBuilding() != null)
                    {
                        m_nMoveToX = GetSupplyCenterAssemblyPositionX();
                        m_nMoveToY = GetSupplyCenterAssemblyPositionY();
                        m_nMoveToZ = GetSupplyCenterAssemblyPositionZ();
                        CallMoveToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                        return MovingToAssemblyPoint;
                    }
                    else
                    {
                        return Nothing;
                    }
                }
            }
            else
            {
                //nie ma obiektow do zaopatrzenia - wracamy do punktu zbiorki
                if (GetSupplyCenterBuilding() != null)
                {
                    m_nMoveToX = GetSupplyCenterAssemblyPositionX();
                    m_nMoveToY = GetSupplyCenterAssemblyPositionY();
                    m_nMoveToZ = GetSupplyCenterAssemblyPositionZ();
                    CallMoveToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                    return MovingToAssemblyPoint;
                }
                else
                {
                    return Nothing;
                }
            }
        }
    }
    
    state PuttingAmmo
    {
        int bIsEnd;
        if (IsPuttingAmmo())
        {
            return PuttingAmmo;
        }
        else
        {
            //sprawdzic czy wrzucono amunicje do biezacego obiektu
            if (WasCurrentObjectSupplied())
            {
                //ok jedziemy do nastepnego
                bIsEnd = false;
                do 
                {
                    if (!NextObjectForSupply())
                    {
                        bIsEnd = true;
                    }
                }
                while (!bIsEnd && !CanCurrentObjectBeSupplied());
                if (!bIsEnd)
                {
                    m_nMoveToX = GetCurrentPutSupplyPositionX();
                    m_nMoveToY = GetCurrentPutSupplyPositionY();
                    m_nMoveToZ = GetCurrentPutSupplyPositionZ();
                    CallMoveToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                    return MovingToObjectForSupply;
                }
                else
                {
                    //nie ma juz obiektow do zaopatrzenia - wracamy do punktu zbiorki
                    if (GetSupplyCenterBuilding() != null)
                    {
                        m_nMoveToX = GetSupplyCenterAssemblyPositionX();
                        m_nMoveToY = GetSupplyCenterAssemblyPositionY();
                        m_nMoveToZ = GetSupplyCenterAssemblyPositionZ();
                        CallMoveToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                        return MovingToAssemblyPoint;
                    }
                    else
                    {
                        return Nothing;
                    }
                }
            }
            else
            {
                //nie wrzucilismy zaopatrzenia do niego bo np. odjechal
                bIsEnd = false;
                while (!bIsEnd && !CanCurrentObjectBeSupplied())
                {
                    if (!NextObjectForSupply())
                    {
                        bIsEnd = true;
                    }
                }
                if (!bIsEnd)
                {
                    m_nMoveToX = GetCurrentPutSupplyPositionX();
                    m_nMoveToY = GetCurrentPutSupplyPositionY();
                    m_nMoveToZ = GetCurrentPutSupplyPositionZ();
                    CallMoveToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                    return MovingToObjectForSupply;
                }
                else
                {
                    //nie ma obiektow do zaopatrzenia - wracamy do punktu zbiorki
                    if (GetSupplyCenterBuilding() != null)
                    {
                        m_nMoveToX = GetSupplyCenterAssemblyPositionX();
                        m_nMoveToY = GetSupplyCenterAssemblyPositionY();
                        m_nMoveToZ = GetSupplyCenterAssemblyPositionZ();
                        CallMoveToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                        return MovingToAssemblyPoint;
                    }
                    else
                    {
                        return Nothing;
                    }
                }
            }
        }
    }
    
    //------------------------------------------------------- 
    state Froozen
    {
        if (IsFroozen())
        {
            state Froozen;
        }
        else
        {
            //!!wrocic do tego co robilismy
            state Nothing;
        }
    }
    
    event OnFreezeForSupplyOrRepair(int nFreezeTicks)
    {
        CallFreeze(nFreezeTicks);
        state Froozen;
        true;
    }
    event OnKilledSupplyCenterBuilding()
    {
        unit uNewSupplyCenter;
        if(!GetSupplyCenterBuilding())//przeniesc do eventu
        {
            uNewSupplyCenter = FindSupplyCenter();        
            if ((uNewSupplyCenter != null) && SetSupplyCenterBuilding(uNewSupplyCenter))
            {
                //pojechac do jego punktu zbiorki o ile nie rozwozimy zaopatrzenia
                m_nMoveToX = GetSupplyCenterAssemblyPositionX();
                m_nMoveToY = GetSupplyCenterAssemblyPositionY();
                m_nMoveToZ = GetSupplyCenterAssemblyPositionZ();
                CallMoveToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                return MovingToAssemblyPoint;
            }
        }
    }
    //------------------------------------------------------- 
    
    command Initialize()
    {
        false;
    }
    
    command Uninitialize()
    {
        //wykasowac referencje
        false;
    }
    
    command SetTransporterSupplyCenter(unit uSupplyCenter) hidden button "translateCommandSetSupplyCntr"
    {
        if(traceMode)TraceD("command SetTransporterSupplyCenter                      \n");
        if (GetSupplyCenterBuilding() != uSupplyCenter)
        {
            if (SetSupplyCenterBuilding(uSupplyCenter))
            {
                //pojechac do jego punktu zbiorki o ile nie rozwozimy zaopatrzenia
                if (!HaveObjectsForSupply())
                {
                    m_nMoveToX = GetSupplyCenterAssemblyPositionX();
                    m_nMoveToY = GetSupplyCenterAssemblyPositionY();
                    m_nMoveToZ = GetSupplyCenterAssemblyPositionZ();
                    CallMoveToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                    state MovingToAssemblyPoint;
                }
                NextCommand(1);
            }
            else
            {
                NextCommand(0);
            }
        }
        else
        {
            if (!HaveObjectsForSupply() && (state != MovingToSupplyCenter))
            {
                m_nMoveToX = GetSupplyCenterAssemblyPositionX();
                m_nMoveToY = GetSupplyCenterAssemblyPositionY();
                m_nMoveToZ = GetSupplyCenterAssemblyPositionZ();
                CallMoveToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                state MovingToAssemblyPoint;
            }
            NextCommand(1);
        }
        true;
    }
    
    //komenda wydawana tylko przez budynek supplyCenter
    command MoveToSupplyCenterForLoading()
    {
        if(traceMode)TraceD("command MoveToSupplyCenterForLoading\n");
        if (!HaveObjectsForSupply() && (state != LoadingAmmo))
        {
            m_nMoveToX = GetSupplyCenterLoadPositionX();
            m_nMoveToY = GetSupplyCenterLoadPositionY();
            m_nMoveToZ = GetSupplyCenterLoadPositionZ();
            //CallMoveAndLandToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
                        CallMoveToPointForce(m_nMoveToX, m_nMoveToY, m_nMoveToZ);//dodane 09.03.2000
            state MovingToSupplyCenter;
        }
        //komenda wewnetrzna z budynku wiec nie wolamy NextCommand
        true;
    }
    
    command Move(int nGx, int nGy, int nLz) hidden button "translateCommandMove" description "translateCommandMoveDescription" hotkey priority 21
    {
        m_nMoveToX = nGx;
        m_nMoveToY = nGy;
        m_nMoveToZ = nLz;
        CallMoveToPoint(m_nMoveToX, m_nMoveToY, m_nMoveToZ);
        state StartMoving;
        true;
    }
    
    command Enter(unit uEntrance) hidden button "translateCommandEnter"
    {
        m_nMoveToX = GetEntranceX(uEntrance);
        m_nMoveToY = GetEntranceY(uEntrance);
        m_nMoveToZ = GetEntranceZ(uEntrance);
        CallMoveInsideObject(uEntrance);
        state StartMoving;
        true;
    }
    
    command Stop() hidden button "translateCommandStop" description "translateCommandStopDescription" hotkey priority 20
    {
        CallStopMoving();
        state Nothing;
        true;
    }
    command Land() button "translateCommandLand" description "translateCommandLandDescription" hotkey priority 31 
    {
        m_nLandCounter = 1;
        if (Land())
        {
            state StartLanding;
        }
        else
        {
            NextCommand(1);
        }
    }
    
    //-------------------------------------------------------
    command SpecialChangeUnitsScript() button "translateCommandChangeScript" description "translateCommandChangeScriptDescription" hotkey priority 254 
    {
        //special command - no implementation
    }
    //-------------------------------------------------------
    command SetLights(int nMode) button lights description "translateCommandStateLightsModeDescription" hotkey priority 204
    {
        if (nMode == -1)
        {
            lights = (lights + 1) % 3;
        }
        else
        {
            assert(nMode == 0);
            lights = nMode;
        }
        SetLightsMode(lights);
    }
    
    //--------------------------------------------------------------------------
    /*    command UserOneParam9(int nMode) hidden button traceMode priority 255
    {
    if (nMode == -1)
    {
    traceMode = (traceMode + 1) % 2;
    }
    else
    {
    assert(nMode == 0);
    traceMode = nMode;
    }
    }*/
}
