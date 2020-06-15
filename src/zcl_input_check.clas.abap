class ZCL_INPUT_CHECK definition
  public
  final
  create public .

public section.

  methods REQUERIED_CHECK
    importing
      !IT_FIELDNAME_TAB type ZBC_TT_FIELDNAME
      !IS_STRUC type ANY
    exporting
      !EV_TYPE type MSGTY
      !EV_MESSAGE type BAPI_MSG .
  methods CONSTRUCTOR
    importing
      !IV_TABNAME type TABNAME .
protected section.
private section.

  data DFIES_TAB type DFIES_TAB .
  data DFIES type DFIES .
ENDCLASS.



CLASS ZCL_INPUT_CHECK IMPLEMENTATION.


  METHOD constructor.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = iv_tabname
      TABLES
        dfies_tab      = dfies_tab
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.


  METHOD requeried_check.
    DATA: ls_fieldname TYPE zbc_s_fieldname,
          lv_reptext   TYPE reptext.

    FIELD-SYMBOLS: <field> TYPE any.

    LOOP AT it_fieldname_tab INTO ls_fieldname .
      TRY.
          IF ls_fieldname-reptext IS INITIAL.
            READ TABLE dfies_tab INTO dfies WITH KEY fieldname = ls_fieldname-fieldname.
            IF sy-subrc EQ 0.
              lv_reptext = ls_fieldname-reptext.
            ENDIF.
          ELSE.
            lv_reptext = ls_fieldname-reptext.

          ENDIF.
          ASSIGN COMPONENT ls_fieldname-fieldname OF STRUCTURE is_struc TO <field>.
          IF sy-subrc EQ 0.
            IF <field> IS INITIAL.
              ev_type    = 'E'.
              ev_message = 'ZBC000:' && lv_reptext  &&  TEXT-001."不能为空
              EXIT.
            ENDIF.
          ELSE.
            ev_type    = 'E'.
            ev_message = 'ZBC000:' && lv_reptext  &&  TEXT-002."字段不存在
            EXIT.
          ENDIF.
        CATCH cx_root INTO DATA(lox_root).
          ev_message = 'ZBC000:' && lox_root->if_message~get_text( )."不能为空
          EXIT.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
