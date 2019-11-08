*----------------------------------------------------------------------------------*
*   Copyright 2014 Adi J. Sieker
*
*   Licensed under the Apache License, Version 2.0 (the "License");
*   you may not use this file except in compliance with the License.
*   You may obtain a copy of the License at
*
*       http://www.apache.org/licenses/LICENSE-2.0
*
*   Unless required by applicable law or agreed to in writing, software
*   distributed under the License is distributed on an "AS IS" BASIS,
*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*   See the License for the specific language governing permissions and
*   limitations under the License.
**---------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS LCX_ERROR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcx_error definition
  inheriting from cx_static_check
  create public .
  public section.
    interfaces if_t100_message .
    aliases text_id
      for if_t100_message~t100key .
    data msgno type syst-msgno read-only.
    data msgid type syst-msgid read-only.
    data msgty type syst-msgty read-only.
    data msgv1 type syst-msgv1 read-only .
    data msgv2 type syst-msgv2 read-only.
    data msgv3 type syst-msgv3 read-only.
    data msgv4 type syst-msgv4 read-only.
    methods constructor
      importing
        !textid like if_t100_message=>t100key optional
        !previous like previous optional
        !msgno type syst-msgno optional
        !msgid type syst-msgid optional
        !msgty type syst-msgty optional
        !msgv1 type syst-msgv1 optional
        !msgv2 type syst-msgv2 optional
        !msgv3 type syst-msgv3 optional
        !msgv4 type syst-msgv4 optional .
    class-methods raise_from_msg
      importing
        !iv_msgid type syst-msgid default sy-msgid
        !iv_msgno type syst-msgno default sy-msgno
        !iv_msgty type syst-msgty default sy-msgty
        !iv_msgv1 type syst-msgv1 default sy-msgv1
        !iv_msgv2 type syst-msgv2 default sy-msgv2
        !iv_msgv3 type syst-msgv3 default sy-msgv3
        !iv_msgv4 type syst-msgv4 default sy-msgv4
      raising
        lcx_error .
    methods if_message~get_text
      redefinition .

  protected section.

  private section.
endclass. "LCX_ERROR definition

*----------------------------------------------------------------------*
*       CLASS LCX_ERROR IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcx_error implementation.

  method constructor.
    call method super->constructor
      exporting
        previous = previous.
    me->msgno = msgno .
    me->msgid = msgid .
    me->msgty = msgty .
    me->msgv1 = msgv1 .
    me->msgv2 = msgv2 .
    me->msgv3 = msgv3 .
    me->msgv4 = msgv4 .
    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.                    "CONSTRUCTOR

  method if_message~get_text.
    if me->msgid is not initial and me->msgno is not initial.
      message id me->msgid type me->msgty number me->msgno
            with me->msgv1 me->msgv2 me->msgv3 me->msgv4
            into result.
    else.
      result = super->if_message~get_text( ).
    endif.
  endmethod.                    "if_message~get_text

  method raise_from_msg.
    raise exception type lcx_error
      exporting
*       textid   =
*       previous =
        msgno    = iv_msgno
        msgid    = iv_msgid
        msgty    = iv_msgty
        msgv1    = iv_msgv1
        msgv2    = iv_msgv2
        msgv3    = iv_msgv3
        msgv4    = iv_msgv4.
  endmethod.                    "raise_from_msg
endclass.                    "LCX_ERROR IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_ISU_STRUCTURE_READ DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_isu_structure_read definition
  create public .
  public section.
    types: tty_efkkpath type table of efkkpath,
           tty_complete_etins_data type table of complete_etins_data,
           tty_te420 type table of te420,
           tty_efkkstruc type table of efkkstruc
           .

    data mt_path type tty_efkkpath read-only .
    data mt_bcont type bcont_t read-only .
    data mt_eabl type eabl_tab read-only .
    data mt_eablg type eablg_tab read-only .
    data mt_eabp type ieabp_t read-only .
    data mt_eadz type isu07_ieadz read-only .
    data mt_eanl type iev_eanl read-only .
    data mt_easti type isu07_ieasti read-only .
    data mt_eastih type isu07_ieastih read-only .
    data mt_eastl type isu07_ieastl read-only .
    data mt_easts type isu07_ieasts read-only .
    data mt_eaus type isu06_t_eaus read-only .
    data mt_eausv type isu06_t_eausv read-only .
    data mt_eein type isu06_t_eein read-only .
    data mt_eeinv type isu06_t_eeinv read-only .
    data mt_eger type iev_eger read-only .
    data mt_egpl type iegpl read-only .
    data mt_ehau type iehau read-only .
    data mt_ekun type isu01_ekun_tab read-only .
    data mt_etdz type isu07_ietdz read-only .
    data mt_etins type tty_complete_etins_data read-only .
    data mt_ettif type isu_iettif read-only .
    data mt_evbs type ieevbs read-only .
    data mt_ever type ieever read-only .
    data mt_ezug type isu07_iezug read-only .
    data mt_ezuz type isu07_iezuz read-only .
    data mt_fkkvkp type fkkvkp1_t read-only .
    data mt_te420 type tty_te420 read-only .
    data mt_te422 type tab_te422 read-only .
    methods add_step
      importing
        !iv_tabfrom type tabfrom
        !iv_tabto type tabto
        !iv_linkid type linkid optional .
    methods read
      importing
        !iv_is_complete type efkkkennzx default space
        !iv_actual type estractual default space
        !iv_structure type efkkkennzx default 'X'
        !iv_ab type efkkab
        !iv_bis type efkkbis
        !iv_text type efkkkennzx default space
        !iv_language type syst-langu default sy-langu
        !iv_progress_display type efkkkennzx default space
      exporting
        !et_struc type tty_efkkstruc
      raising
        lcx_error .
    methods add_bcont
      importing
        !is_bcont type bcont .
    methods add_eabl
      importing
        !is_eabl type eabl .
    methods add_eablg
      importing
        !is_eablg type eablg .
    methods add_eabp
      importing
        !is_eabp type eabp .
    methods add_eadz
      importing
        !is_eadz type eadz .
    methods add_eanl
      importing
        !is_eanl type v_eanl .
    methods add_easti
      importing
        !is_easti type easti .
    methods add_eastih
      importing
        !is_eastih type eastih .
    methods add_eastl
      importing
        !is_eastl type eastl .
    methods add_easts
      importing
        !is_easts type easts .
    methods add_eaus
      importing
        !is_eaus type eaus .
    methods add_eausv
      importing
        !is_eausv type eausv .
    methods add_eein
      importing
        !is_eein type eein .
    methods add_eeinv
      importing
        !is_eeinv type eeinv .
    methods add_eger
      importing
        !is_eger type v_eger .
    methods add_egpl
      importing
        !is_egpl type egpl .
    methods add_ehau
      importing
        !is_ehau type ehau .
    methods add_ekun
      importing
        !is_ekun type isu01_ekun .
    methods add_etdz
      importing
        !is_etdz type etdz .
    methods add_etins
      importing
        !is_etins type complete_etins_data .
    methods add_ettif
      importing
        !is_ettif type ettif .
    methods add_evbs
      importing
        !is_evbs type evbs .
    methods add_ever
      importing
        !is_ever type ever .
    methods add_ezug
      importing
        !is_ezug type ezug .
    methods add_ezuz
      importing
        !is_ezuz type ezuz .
    methods add_fkkvkp
      importing
        !is_fkkvkp type fkkvkp1 .
    methods add_te420
      importing
        !is_te420 type te420 .
    methods add_te422
      importing
        !is_te422 type te422 .

  protected section.

  private section.
endclass. "lcl_ISU_STRUCTURE_READ definition

*----------------------------------------------------------------------*
*       CLASS lcl_ISU_STRUCTURE_READ IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_isu_structure_read implementation.

  method add_bcont.
    insert is_bcont into table me->mt_bcont.
  endmethod.                    "add_BCONT

  method add_eabl.
    insert is_eabl into table me->mt_eabl.
  endmethod.                    "add_EABL

  method add_eablg.
    insert is_eablg into table me->mt_eablg.
  endmethod.                    "add_EABLG

  method add_eabp.
    insert is_eabp into table me->mt_eabp.
  endmethod.                    "add_EABP

  method add_eadz.
    insert is_eadz into table me->mt_eadz.
  endmethod.                    "add_EADZ

  method add_eanl.
    insert is_eanl into table me->mt_eanl.
  endmethod.                    "add_EANL

  method add_easti.
    insert is_easti into table me->mt_easti.
  endmethod.                    "add_EASTI

  method add_eastih.
    insert is_eastih into table me->mt_eastih.
  endmethod.                    "add_EASTIH

  method add_eastl.
    insert is_eastl into table me->mt_eastl.
  endmethod.                    "add_EASTL

  method add_easts.
    insert is_easts into table me->mt_easts.
  endmethod.                    "add_EASTS

  method add_eaus.
    insert is_eaus into table me->mt_eaus.
  endmethod.                    "add_EAUS

  method add_eausv.
    insert is_eausv into table me->mt_eausv.
  endmethod.                    "add_EAUSV

  method add_eein.
    insert is_eein into table me->mt_eein.
  endmethod.                    "add_EEIN

  method add_eeinv.
    insert is_eeinv into table me->mt_eeinv.
  endmethod.                    "add_EEINV

  method add_eger.
    insert is_eger into table me->mt_eger.
  endmethod.                    "add_EGER

  method add_egpl.
    insert is_egpl into table me->mt_egpl.
  endmethod.                    "add_EGPL

  method add_ehau.
    insert is_ehau into table me->mt_ehau.
  endmethod.                    "add_EHAU

  method add_ekun.
    insert is_ekun into table me->mt_ekun.
  endmethod.                    "add_EKUN

  method add_etdz.
    insert is_etdz into table me->mt_etdz.
  endmethod.                    "add_ETDZ

  method add_etins.
    insert is_etins into table me->mt_etins.
  endmethod.                    "add_ETINS

  method add_ettif.
    insert is_ettif into table me->mt_ettif.
  endmethod.                    "add_ETTIF

  method add_evbs.
    insert is_evbs into table me->mt_evbs.
  endmethod.                    "add_EVBS

  method add_ever.
    insert is_ever into table me->mt_ever.
  endmethod.                    "add_EVER

  method add_ezug.
    insert is_ezug into table me->mt_ezug.
  endmethod.                    "add_EZUG

  method add_ezuz.
    insert is_ezuz into table me->mt_ezuz.
  endmethod.                    "add_EZUZ

  method add_fkkvkp.
    insert is_fkkvkp into table me->mt_fkkvkp.
  endmethod.                    "add_FKKVKP

  method add_te420.
    insert is_te420 into table me->mt_te420.
  endmethod.                    "add_TE420

  method add_te422.
    insert is_te422 into table me->mt_te422.
  endmethod.                    "add_TE422

  method add_step.
    data: ls_step type efkkpath.
    ls_step-tabfrom = iv_tabfrom.
    ls_step-tabto = iv_tabto.
    ls_step-linkid = iv_linkid.
    insert ls_step into table me->mt_path.
  endmethod.                    "add_step

  method read.
    call function 'ISU_STRUCTURE_READ'
      exporting
        x_is_complete      = iv_is_complete
        x_actual           = iv_actual
        x_structure        = iv_structure
        x_ab               = iv_ab
        x_bis              = iv_bis
        x_text             = iv_text
        x_language         = iv_language
        x_progress_display = iv_progress_display
      tables
        xt_path            = me->mt_path
        yt_struc           = et_struc
        t_bcont            = me->mt_bcont[]
        t_eabl             = me->mt_eabl[]
        t_eablg            = me->mt_eablg[]
        t_eabp             = me->mt_eabp[]
        t_eadz             = me->mt_eadz[]
        t_eanl             = me->mt_eanl[]
        t_easti            = me->mt_easti[]
        t_eastih           = me->mt_eastih[]
        t_eastl            = me->mt_eastl[]
        t_easts            = me->mt_easts[]
        t_eaus             = me->mt_eaus[]
        t_eausv            = me->mt_eausv[]
        t_eein             = me->mt_eein[]
        t_eeinv            = me->mt_eeinv[]
        t_eger             = me->mt_eger[]
        t_egpl             = me->mt_egpl[]
        t_ehau             = me->mt_ehau[]
        t_ekun             = me->mt_ekun[]
        t_etdz             = me->mt_etdz[]
        t_etins            = me->mt_etins[]
        t_ettif            = me->mt_ettif[]
        t_evbs             = me->mt_evbs[]
        t_ever             = me->mt_ever[]
        t_ezug             = me->mt_ezug[]
        t_ezuz             = me->mt_ezuz[]
        t_fkkvkp           = me->mt_fkkvkp[]
        t_te420            = me->mt_te420[]
        t_te422            = me->mt_te422[]
      exceptions
        path_invalid       = 1
        date_invalid       = 2
        selection_failed   = 3
        others             = 4.
    if sy-subrc <> 0.
      lcx_error=>raise_from_msg( ).
    endif.
  endmethod.                    "read
endclass. "lcl_ISU_STRUCTURE_READ implementation
