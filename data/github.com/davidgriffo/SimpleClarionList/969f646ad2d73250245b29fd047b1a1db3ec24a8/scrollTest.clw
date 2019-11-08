
  PROGRAM


! Created with Clarion 10.0
! User: davidgriffiths
! 
!  An example of  :
!   * filtering a list of records  
!   * stepping through a list  
!                    


    MAP      
loadList        PROCEDURE(CustomerListType fromQ, CustomerListType toQ,string filter)
randomName      PROCEDURE(),STRING

    END    
    INCLUDE('KEYCODES.CLW')

CustomerListType                QUEUE,type 
Customer                                string(100)
id                                      LONG
                                end
CustomerList                    QUEUE(CustomerListType)      
                                end

ListQueue                       QUEUE(CustomerListType)
                                end


SearchName                      CSTRING(20)  
SearchNameCopy                  CSTRING(20)  
searchType                      string ('Filter')

Window                          WINDOW('Scroll test window'),AT(,,300,191),GRAY,IMM,ICON(ICON:Frame), |
                                        FONT('Tahoma',11,,FONT:regular),TIMER(10)
                                    LIST,AT(11,22,284,161),USE(?LIST1),VSCROLL,FROM(LISTQueue)
                                    PROMPT('Find'),AT(10,6),USE(?PROMPTFind)
                                    ENTRY(@s20),AT(34,6),USE(SearchName)
                                    OPTION,AT(150,1,136,19),USE(searchType),BOXED
                                        RADIO('Step'),AT(161,6),USE(?FilterTypeStep),VALUE('Step')
                                        RADIO('Filter'),AT(206,6,40,10),USE(?FilterTypeFilter),VALUE('Filter')
                                    END
                                END

i                               LONG
    CODE   
        ! create a dummy list of 100 customers
        loop i =  1 to 100   
            CustomerList.id = i
            CustomerList.Customer = randomName()
            add(CustomerList)
        END     
        sort(CustomerList,CustomerList.Customer)   
        ! copy this list to the window list
        Loadlist(CustomerList,ListQueue,'')
        OPEN(Window)
        ACCEPT   
            case EVENT()
            of EVENT:Accepted
                case FIELD()
                of ?searchType
                    if searchType = 'Filter'
                        unhide(?PROMPTFind)
                        UNHIDE(?SearchName)
                        SELECT(?SearchName)
                    ELSE   
                        ! the step entry is built into the list 
                        ! there is no programming needed
                        hide(?PROMPTFind)
                        HIDE(?SearchName) 
                        select(?LIST1)
                    END
                END
            
            of EVENT:Timer
                UPDATE 
                ! check if changed
                if  searchType = 'Filter' AND  SearchNameCopy <>  SearchName 
                    SearchNameCopy =  SearchName     
                    if searchType = 'Filter'
                        loadList(CustomerList,ListQueue,SearchName)
                    END
                    
                END
                    
            END
        END
!!! <Summary>
!!!  Copy one list to another depending on the supplied filter
!!! </Summary>    
loadList                        PROCEDURE  (CustomerListType fromQ ,CustomerListType toQ,string filter)       
x                                  long
    code
        free(toQ)
        loop x = 1 to records(fromQ) 
            get(fromQ,x)  
            if  filter = ''  or   instring(UPPER(filter),UPPER(fromQ.Customer),1,1)
                toQ  :=: fromQ
                add(toQ)
            END
            
        end   
!!! <Summary>
!!!  return a random name
!!! </Summary> 
RandomName                      PROCEDURE()  
RName                               cstring(100)
    code
        RName = chr(val('A') + random(0,25)) ! first letter capital
        loop RANDOM(6,13) TIMES    
            RName = RName & chr(val('a') + random(0,25))
        END       
        return RName
