class zcl_debug_obj_graph_factory definition public final create public .

  public section.
    class-methods create importing i_max_tab_lines type i default 200
                         returning value(r_result) type ref to zcl_debug_obj_graph_factory.

    methods: create_graph importing root         type string
                          returning value(graph) type ref to zcl_abap_graph.

    data: max_tab_lines type i read-only,
          logs          type string read-only.

  private section.
    constants: c_headerbg    type string value 'lavender',
               c_privatebg   type string value 'lightsalmon',
               c_protectedbg type string value 'khaki'.

    types: begin of ty_type_desc,
             name  type string,
             local type abap_bool,
           end of ty_type_desc,
           begin of ty_data_desc,
             name          type string,
             descr         type ref to cl_tpda_script_data_descr,
             refname       type string,
             valuetext     type string,
             typedesc      type ty_type_desc,
             metatype      type tpda_scr_quick_info-metatype,
             hascomponents type abap_bool,
             istable       type abap_bool,
             isreference   type abap_bool,
             haschildren   type abap_bool,
           end of ty_data_desc,
           begin of ty_component,
             compname   type string,
             value      type string,
             desc       type ty_data_desc,
             visibility type i,
           end of ty_component,
           tt_component type table of ty_component with default key.

    constants: c_public    type i value 0,
               c_protected type i value 1,
               c_private   type i value 2,
               c_dummy     type string value 'DUMMY'.
    data: graph      type ref to zcl_abap_graph.

    methods create_node_new importing desc        type zcl_debug_obj_graph_factory=>ty_data_desc
                            returning value(node) type ref to zif_abap_graph_node.

    methods get_or_create_node importing name          type string
                               returning value(nodeid) type string.

    methods get_node_id importing name      type string
                        returning value(id) type string.


    methods escapevalue importing original       type string
                        returning value(escaped) type string.

    methods get_data_desc importing name        type string
                          returning value(desc) type ty_data_desc.

    methods create_table_node      importing desc        type zcl_debug_obj_graph_factory=>ty_data_desc
                                   returning value(node) type ref to zif_abap_graph_node.

    methods create_record_node      importing desc        type zcl_debug_obj_graph_factory=>ty_data_desc
                                    returning value(node) type ref to zif_abap_graph_node.

    methods create_simple_node      importing desc        type zcl_debug_obj_graph_factory=>ty_data_desc
                                    returning value(node) type ref to zif_abap_graph_node.

    methods get_components      importing desc              type zcl_debug_obj_graph_factory=>ty_data_desc
                                returning value(components) type zcl_debug_obj_graph_factory=>tt_component.

    methods log      importing  entry type string.

    methods add_record_component importing record             type ref to zcl_abap_graph_node_record
                                           component          type zcl_debug_obj_graph_factory=>ty_component
                                 returning value(componentid) type string.

    methods get_value    importing desc            type ty_data_desc
                         returning value(r_result) type string.

    methods get_head_label    importing desc            type ty_data_desc
                              returning value(r_result) type string.

    methods decode_visibility importing acckind         type i
                              returning value(r_result) type i.

    methods decodetype importing typename  type string
                                 desc      type ty_data_desc
                       returning value(td) type ty_type_desc.

    methods viscolor   importing visibility      type i
                       returning value(r_result) type string.

    methods bold       importing base            type string
                       returning value(r_result) type string.

    methods italics    importing base            type string
                       returning value(r_result) type string.
    methods underline  importing base            type string
                       returning value(r_result) type string.
    methods add_table_cell importing table              type ref to zcl_abap_graph_node_table
                                     desc               type zcl_debug_obj_graph_factory=>ty_data_desc
                                     line               type i
                                     value(compname)    type string
                           returning value(componentid) type string.

ENDCLASS.



CLASS ZCL_DEBUG_OBJ_GRAPH_FACTORY IMPLEMENTATION.


  method add_record_component.
    data: id       type string,
          partid   type string,
          color    type string,
          compname type string,
          value    type string.

    id = component-desc-name .

    if component-desc-haschildren = abap_true.
      partid = component-compname.
      value = get_value( component-desc ).
    else.
      partid = ''.
      value =  component-value .
    endif.
    compname = bold( component-compname ).

    color = viscolor( component-visibility ).
    componentid = record->addcomponent(
        name        = compname
        value       = value
        escape      = abap_false
        bgcolor     = color
        partid      = partid ).

  endmethod.


  method add_table_cell.
    data: partid   type string,
          value    type string,
          linkedid type string.
    log( |R{ desc-refname }_C{ compname }_L{ line } | ).
    if desc-refname <> ''.
      if compname = ''.
        partid = |l{ line }|.
      else.
        partid = |l{ line }c{ compname }|.
      endif.
    else.
      partid = ''.
    endif.
    if compname = ''.
      compname = c_dummy.
    endif.

    value = get_value( desc ).

    log( |R{ desc-refname }_C{ componentid }_V{ value } | ).

    componentid = table->setcell(
        columnid    = compname
        row         = line
        value       = value
        escape      = abap_false
        partid      = partid ).

    if desc-refname <> '' and componentid <> '' .
      linkedid = get_or_create_node( desc-refname ).
      if linkedid <> ''.
        table->linkto( destination = linkedid source = componentid ).
      endif.
    endif.

  endmethod.


  method bold.
    if base <> ''.
      concatenate '<b>' base '</b>' into r_result.
    endif.
  endmethod.


  method create.

    create object r_result.

    r_result->max_tab_lines = i_max_tab_lines.

  endmethod.


  method create_graph.
    data: desc type zcl_debug_obj_graph_factory=>ty_data_desc.

    graph = zcl_abap_graph=>create( ).
    me->graph = graph.

    desc = get_data_desc( root ).

    if desc is initial.
      message 'Variable not found' type 'S'.
    else.
      create_node_new( desc ).
    endif.

  endmethod.


  method create_node_new.

    if desc-istable = abap_true.
      node = create_table_node( desc ).
    elseif desc-hascomponents = abap_true.
      node = create_record_node( desc ).
    else.
      node = create_simple_node( desc ).
    endif.

  endmethod.


  method create_record_node.
    data: value       type string,
          id          type string,
          linkedid    type string,
          components  type tt_component,
          record      type ref to zcl_abap_graph_node_record,
          componentid type string,
          ex          type ref to cx_root.
    field-symbols: <component> like line of components.

    try.
        components = get_components( desc ).

        value = get_head_label( desc ).
        id = desc-name .
        node = record = zcl_abap_graph_node_record=>create( graph = graph id = id label = value escape = abap_false ).
        record->headerattr->set( name = 'bgcolor' value = c_headerbg ).

        loop at components assigning <component>.
          componentid = add_record_component( record = record component = <component> ).

          if componentid <> ''.
            linkedid = get_or_create_node( <component>-desc-refname ).
            if linkedid <> ''.
              node->linkto( destination = linkedid source = componentid ).
            endif.
          endif.

        endloop.
      catch cx_root into ex.
        log( |create_record_node exception { ex->get_text( ) } | ).
    endtry.
  endmethod.


  method create_simple_node.
    data: value    type string,
          id       type string,
          linkedid type string.

    value = desc-valuetext.
    id =  desc-name .
    node = zcl_abap_graph_node_simple=>create( graph = graph id = id label = value ).

    if desc-haschildren = abap_true.
      linkedid = get_or_create_node( desc-refname ).
      if linkedid <> ''.
        node->linkto( linkedid ).
      endif.
    endif.

  endmethod.


  method create_table_node.
    data: value      type string,
          id         type string,
          components type tt_component,
          tabdesc    type ref to cl_tpda_script_tabledescr,
          table      type ref to zcl_abap_graph_node_table,
          linename   type string,
          line       type i,
          linedesc   type zcl_debug_obj_graph_factory=>ty_data_desc,
          ex         type ref to cx_root.
    field-symbols: <component> like line of components.


    try.
        tabdesc ?= desc-descr.
        id = desc-name.
        value = get_head_label( desc ).
        log( |table { id }__{ value }| ).
        node = table = zcl_abap_graph_node_table=>create( graph = graph
                                                          id = id
                                                          label = value
                                                          escape = abap_false ).
        table->headerattr->set( name = 'bgcolor' value = c_headerbg ).
        table->titleattr->set( name = 'bgcolor' value = c_headerbg ).

        do tabdesc->linecnt( ) times.
          line = sy-index.
          log( | table line { line } | ).
          linename = |{ desc-name }[{ line }]|.
          linedesc = get_data_desc( linename ).
          if line = 1 or not components is initial.
            components = get_components( linedesc ).
            if line = 1.
              "header
              if components is initial.
                table->setcolumn( id = c_dummy ).
              else.
                loop at components assigning <component>.
                  value = escapevalue( <component>-compname ).
                  value = bold( value ).
                  table->setcolumn( id = <component>-compname name = value ).
                endloop.
              endif.
            endif.
          endif.

          if components is initial.
            add_table_cell( table = table line = line desc = linedesc compname = '' ).
          else.
            "actual components of a line
            loop at components assigning <component>.
              add_table_cell( table = table line = line desc = <component>-desc  compname = <component>-compname ).
            endloop.
          endif.
        enddo.
      catch cx_root into ex.
        log( |create_table_node exception { ex->get_text( ) } | ).
    endtry.

  endmethod.


  method decodetype.
    data: base  type string,
          extra type string.

    td-local = abap_true.
    td-name = typename.
    find regex '^\\TYPE=(.+)' in typename submatches base.
    if sy-subrc = 0.
      if base(1) = '%'.
        case desc-metatype.
          when cl_tpda_control=>mt_tab.
            base = 'table'.
          when cl_tpda_control=>mt_struct.
            base = 'structure'.
          when cl_tpda_control=>mt_class.
            base = 'class'.
          when others.
            base = 'type'.
        endcase.
        concatenate 'local ' base '' into base respecting blanks.
      else.
        td-local = abap_false.
      endif.
      td-name = base.
      return.
    endif.
    find regex '^\\PROGRAM=[^\\]*\\CLASS=([^\\]+)' in typename submatches base.
    if sy-subrc = 0.
      td-name = base.
      return.
    endif.
    find regex '^\\PROGRAM=[^\\]*\\CLASS=([^\\]+)' in typename submatches base.
    if sy-subrc = 0.
      td-name = base.
      return.
    endif.
    find regex '^\\CLASS-POOL=([^\\]+)\\CLASS=([^\\]+)' in typename submatches base extra.
    if sy-subrc = 0.
      concatenate base extra into td-name separated by space.
      return.
    endif.
  endmethod.


  method decode_visibility.
    case acckind.
      when if_tpda_control=>ak_private.
        r_result =  c_private.
      when if_tpda_control=>ak_protected.
        r_result  = c_protected.
    endcase.
  endmethod.


  method escapevalue.
    escaped = cl_http_utility=>escape_html( original ).
  endmethod.


  method get_components.
    data: odesc         type ref to cl_tpda_script_objectdescr,
          sdesc         type ref to cl_tpda_script_structdescr,
          tdesc         type ref to cl_tpda_script_tabledescr,
          objattributes type tpda_script_object_attribut_it,
          scomponents   type tpda_script_struc_componentsit,
          refname       type string,
          match_offset  type i,
          ex            type ref to cx_root,
          basename      type string.
    field-symbols: <scomponent> like line of scomponents,
                   <component>  like line of components,
                   <attribute>  like line of objattributes.
    try.

        case desc-metatype.
          when cl_tpda_script_data_descr=>mt_object.
            odesc ?= desc-descr.
            objattributes = odesc->attributes( ).
          when cl_tpda_script_data_descr=>mt_struct."struct
            sdesc ?= desc-descr.
            sdesc->components( importing p_components_it = scomponents ).
            find regex '\*$' in desc-name match offset match_offset.
            if match_offset = 0.
              basename = desc-name.
            else.
              basename = desc-name(match_offset).
            endif.
            basename = desc-name.
          when cl_tpda_script_data_descr=>mt_tab.
            tdesc ?= desc-descr.
            if tdesc->linecnt( ) > 0.
              log( |get_components probable casting exception after this line | ).
              sdesc ?= tdesc->get_line_handle( 1 ).
              sdesc->components( importing p_components_it = scomponents ).
              concatenate desc-name '[1]'  into  basename.
            endif.
        endcase.

        loop at objattributes assigning <attribute>.
          append initial line to components assigning <component>.
          <component>-compname = <attribute>-name.
          concatenate desc-name '-'  <attribute>-name into refname.
          <component>-desc = get_data_desc( refname ).
          <component>-visibility = decode_visibility( <attribute>-acckind ).
          <component>-value = get_value( <component>-desc ).
        endloop.

        loop at scomponents assigning <scomponent>.
          append initial line to components assigning <component>.
          <component>-compname = <scomponent>-compname.
          <component>-visibility = c_public.
          concatenate basename '-'  <component>-compname into refname.
          <component>-desc = get_data_desc( refname ).
          <component>-value = get_value( <component>-desc ).
        endloop.

      catch cx_root into ex.
        log( |get_components { ex->get_text( ) } | ).
    endtry.
    if desc-metatype = cl_tpda_script_data_descr=>mt_object.
      sort components by visibility compname ascending.
    endif.
  endmethod.


  method get_data_desc.
    data: elem   type ref to cl_tpda_script_elemdescr,
          string type ref to cl_tpda_script_stringdescr,
          table  type ref to cl_tpda_script_tabledescr,
          odesc  type ref to cl_tpda_script_objectdescr,
          info   type tpda_scr_quick_info,
          ex     type ref to cx_root.

    field-symbols: <symdataref> type tpda_sys_symbdatref,
                   <symobjref>  type tpda_sys_symbobjref.

    desc-name = name.

    try.
        desc-descr = cl_tpda_script_data_descr=>factory( name ).
        info  = cl_tpda_script_data_descr=>get_quick_info( name ).
        desc-metatype = info-metatype.
        case info-metatype.
          when cl_tpda_script_data_descr=>mt_string.
            string ?= desc-descr.
            desc-valuetext = string->value( ).
            desc-valuetext = cl_http_utility=>escape_html( desc-valuetext ).
          when cl_tpda_script_data_descr=>mt_simple.
            elem ?= desc-descr.
            desc-valuetext = elem->value( ).
            desc-valuetext = cl_http_utility=>escape_html( desc-valuetext ).
          when cl_tpda_script_data_descr=>mt_struct.
            "            desc-valuetext = cl_http_utility=>escape_html( info-abstypename ).
            desc-typedesc = decodetype( typename = info-abstypename desc = desc ).
            desc-hascomponents = abap_true.
            desc-haschildren = abap_true.
            desc-refname = name.
          when cl_tpda_script_data_descr=>mt_tab.
            table ?= desc-descr.
            if table->linecnt( ) > 0.
              desc-haschildren = abap_true.
              desc-istable = abap_true.
              desc-valuetext = '<table>'.
              desc-refname = name.
            else.
              desc-valuetext = '<emptytable>'.
            endif.
            desc-typedesc = decodetype( typename = info-abstypename desc = desc ).
          when cl_tpda_script_data_descr=>mt_datref.
            assign info-quickdata->* to <symdataref>.
            find regex '^\{[A-Z]:initial\}$' in <symdataref>-instancename ignoring case.
            desc-isreference = abap_true.
            if sy-subrc <> 0.
              desc-haschildren = abap_true.
              desc-refname = <symdataref>-instancename.
              desc-valuetext = '<reference>'.
            else.
              desc-valuetext = '<null reference>'.
            endif.
          when cl_tpda_script_data_descr=>mt_object.
            desc-haschildren = abap_true.
            desc-hascomponents = abap_true.
            odesc ?= desc-descr.
            desc-valuetext = odesc->classname( ).
            desc-typedesc = decodetype( typename = desc-valuetext desc = desc ).
          when cl_tpda_script_data_descr=>mt_objref.
            assign info-quickdata->* to <symobjref>.
            if <symobjref>-instancename <> '{O:initial}'.
              desc-haschildren = abap_true.
              desc-refname = <symobjref>-instancename.
              desc-valuetext = '<object_ref>'.
            else.
              desc-valuetext = '<null object_ref>'.
            endif.
            desc-isreference = abap_true.
        endcase.
      catch cx_root into ex.
        clear desc.
        log( |get_data_desc exception { ex->get_text( ) } | ).
    endtry.

  endmethod.


  method get_head_label.
    if    desc-istable       = abap_true
       or desc-hascomponents = abap_true
       or desc-isreference   = abap_true.
      r_result = escapevalue( desc-typedesc-name ).
      if desc-typedesc-local = abap_true.
        r_result = italics( r_result ).
      endif.
    endif.
    if r_result is initial.
      r_result = escapevalue( desc-valuetext ).
    endif.
  endmethod.


  method get_node_id.
    id = name.
  endmethod.


  method get_or_create_node.
    data: desc type zcl_debug_obj_graph_factory=>ty_data_desc,
          node type ref to zif_abap_graph_node.

    node = graph->get_node( name ).

    if not node is bound.
      "look up cache
      desc = get_data_desc( name ).
      node = create_node_new( desc ).
    endif.

    if node is bound.
      nodeid = node->id.
    endif.

  endmethod.


  method get_value.
    if    desc-istable       = abap_true
       or desc-hascomponents = abap_true
       or desc-isreference   = abap_true.
      r_result = escapevalue( desc-typedesc-name ).
      if desc-typedesc-local = abap_true.
        r_result = italics( r_result ).
      endif.
      r_result = underline(  r_result ).
      if r_result is initial.
        r_result = escapevalue( desc-valuetext ).
      endif.
    endif.

    if r_result is initial.
      r_result = escapevalue( desc-valuetext ).
    endif.

  endmethod.


  method italics.
    if base <> ''.
      concatenate '<i>' base '</i>' into r_result.
    endif.
  endmethod.


  method log.
    logs = |{ logs }{ cl_abap_char_utilities=>cr_lf }{ entry }|.
  endmethod.


  method underline.
    if base <> ''.
      concatenate '<u>' base '</u>' into r_result.
    endif.
  endmethod.


  method viscolor.

    case visibility.
      when c_protected.
        r_result = c_protected.
      when c_private.
        r_result = c_privatebg.
    endcase.

  endmethod.
ENDCLASS.
