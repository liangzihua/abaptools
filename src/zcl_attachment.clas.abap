class ZCL_ATTACHMENT definition
  public
  final
  create public .

public section.

  constants FILETYPE_DOC type STRING value 'DOC' ##NO_TEXT.
  constants FILETYPE_DOCX type STRING value 'DOCX' ##NO_TEXT.
  constants FILETYPE_XLS type STRING value 'XLS ' ##NO_TEXT.
  constants FILETYPE_XLSX type STRING value 'XLSX' ##NO_TEXT.
  constants FILETYPE_PPT type STRING value 'PPT ' ##NO_TEXT.
  constants FILETYPE_PPTX type STRING value 'PPTX' ##NO_TEXT.
  constants FILETYPE_PDF type STRING value 'PDF' ##NO_TEXT.
  constants FILETYPE_TXT type STRING value 'TXT' ##NO_TEXT.
  constants FILETYPE_CSV type STRING value 'CSV' ##NO_TEXT.
  constants FILETYPE_BMP type STRING value 'BMP' ##NO_TEXT.
  constants FILETYPE_JPG type STRING value 'JPG' ##NO_TEXT.
  constants FILETYPE_PNG type STRING value 'PNG' ##NO_TEXT.
  constants FILETYPE_TIF type STRING value 'TIF' ##NO_TEXT.
  constants FILETYPE_GIF type STRING value 'GIF' ##NO_TEXT.
  constants APP_TYPE_HTML type STRING value 'HTML' ##NO_TEXT.
  constants APP_TYPE_OLE type STRING value 'OLE' ##NO_TEXT.
  data TOKEN type ZBC_S_TOKEN .

  methods DIRECTORY_BROWSE
    returning
      value(DIR) type STRING .
  class-methods PROGRESS_INDICATOR
    importing
      !PERCENTAGE type I default 0
      !TEXT type ANY .
  methods FILE_DOWNLOAD
    importing
      !IT_FILENAME type ZBC_TT_FILETABLE optional
      !IV_DIRECT_DOWN type CHAR1 default ''
    returning
      value(RT_RETURN) type ZBC_TT_FDOWNLOAD_RETURN
    exceptions
      ERROR .
  methods FILE_OPEN_DIALOG
    importing
      !MULTISELECTION type ABAP_BOOL default 'X'
    exporting
      value(FILETABLE) type FILETABLE .
  methods FILE_SAVE_DIALOG
    importing
      !INIT_FILENAME type STRING .
  methods CONSTRUCTOR .
  methods GET_TOKEN
    returning
      value(RV_TOKEN) type ZBC_S_TOKEN
    exceptions
      ERROR .
  methods FILE_UPLOAD
    importing
      !FILETABLE type ZBC_TT_FILETABLE
    returning
      value(RETURN) type ZBC_TT_FUPLOAD_RETURN
    exceptions
      ERROR .
  methods FILE_DISPLAY
    importing
      !IT_FILENAME type ZBC_TT_FILETABLE
    returning
      value(RT_RETURN) type ZBC_TT_FDOWNLOAD_RETURN .
  methods ATTACHMENT_UPLOAD
    importing
      !IV_FILENAME type STRING optional
      !IV_MULTISELECTION type ABAP_BOOL default 'X'
    changing
      !CV_DOCKEY type ANY .
  methods ATTACHMENT_DOWNLOAD
    importing
      !IV_DOCKEY type ANY
      !IV_FILENAME type STRING optional .
  methods ATTACHMENT_DISPLAY
    importing
      !IV_DOCKEY type ANY
      !IV_FILENAME type STRING optional .
protected section.
private section.

  data DOCU_CONTAINER type ref to CL_GUI_DIALOGBOX_CONTAINER .
  data MT_FILELIST type ZBC_TT_FILELIST_RETURN .
  data HTML_VIEWER type ref to CL_GUI_HTML_VIEWER .
  data COL_POS type LVC_COLPOS .
  data DIALOG_CONTAINER type ref to CL_GUI_DIALOGBOX_CONTAINER .
  data GRID type ref to ZCL_GUI_ALV_GRID .
  data FILE_TABLE type ZBC_S_FILETABLE .
  data FULLNAME type STRING .
  data FILENAME type STRING .
  data FILEPATH type STRING .
  constants C_AUTHOR_KEY type STRING value 'Authorization' ##NO_TEXT.
  constants C_TOKEN_KEY type STRING value 'Blade-Auth' ##NO_TEXT.
  constants C_JTI_KEY type STRING value 'jti' ##NO_TEXT.
  data TOKEN_CONFIG type ZHRINTBST001 .
  data FILE_UPLOAD_CONFIG type ZHRINTBST001 .
  data FILE_DOWNLOAD_CONFIG type ZHRINTBST001 .

  methods GET_APPLICATION_TYPE
    importing
      !IV_DOCKEY type STRING
    exporting
      !EX_TYPE type TEXT20
      !EX_SUBTYPE type TEXT20
    returning
      value(RT_APP_TYPE) type STRING .
  methods HTML_SHOW
    importing
      !IV_DATA type XSTRING
      !IV_TYPE type TEXT20
      !IV_SUBTYPE type TEXT20
      !IV_FILENAME type STRING .
  methods OLE_SHOW
    importing
      !IV_DATA type XSTRING
      !IV_TYPE type TEXT20
      !IV_SUBTYPE type TEXT20
      !IV_FILENAME type STRING .
  methods BUILD_FCAT
    importing
      !IV_FIELDNAME type LVC_FNAME
      !IV_REPTEXT type ANY optional
      !IV_REF_TABLE type LVC_RTNAME optional
      !IV_REF_FIELD type LVC_RFNAME optional
      !IV_KEY type LVC_KEY optional
      !IV_NOZERO type LVC_NOZERO optional
      !IV_DOSUM type LVC_DOSUM optional
      !IV_CONVEXIT type CONVEXIT optional
      !IV_EDIT type LVC_EDIT optional
      !IV_CHECKBOX type LVC_CHECKB optional
      !IV_NO_OUT type LVC_NOOUT optional
      !IV_HOTSPOT type LVC_HOTSPT optional
    changing
      !CT_FCAT type LVC_T_FCAT .
  methods DOCUMENT_SHOW
    importing
      !IV_DOCKEY type STRING
      !IV_FILENAME type STRING optional .
  methods FILELIST_SHOW
    importing
      !IT_DATA type ZBC_TT_FDOWNLOAD_RETURN .
  methods HANDLE_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
  methods HANDLE_HOTSPOT_CLICK
    for event HOTSPOT_CLICK of ZCL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  methods HANDLE_TOOLBAR
    for event TOOLBAR of ZCL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods GUI_DOWNLOAD
    importing
      !FILENAME type STRING
      value(DATA) type XSTRING
    exceptions
      ERROR .
  methods GUI_UPLOAD
    importing
      !FILENAME type STRING
    returning
      value(DATA) type XSTRING
    exceptions
      ERROR .
ENDCLASS.



CLASS ZCL_ATTACHMENT IMPLEMENTATION.


  METHOD ATTACHMENT_DISPLAY.


  DATA: lob_attchment TYPE REF TO zcl_attachment,
        lt_return     TYPE zbc_tt_fdownload_return,
        ls_return     TYPE zbc_s_fdownload_return,
        lt_filetable  TYPE zbc_tt_filetable,
        ls_filetable  LIKE LINE OF lt_filetable.


  CLEAR: ls_filetable.
  ls_filetable-dockey   = iv_dockey.
  ls_filetable-filename = iv_filename.
  APPEND ls_filetable TO lt_filetable.

  lt_return = file_display( EXPORTING it_filename    = lt_filetable ).


  ENDMETHOD.


  METHOD attachment_download.
    DATA: lt_filetable TYPE zbc_tt_filetable,
          ls_filetable TYPE zbc_s_filetable,
          lt_return    TYPE zbc_tt_fdownload_return,
          ls_return    TYPE zbc_s_fdownload_return,
          lv_type      TYPE sy-msgty,
          lv_message   TYPE bapi_msg.


    CLEAR: ls_filetable.
    ls_filetable-dockey   = iv_dockey   .
    ls_filetable-filename = iv_filename .
    APPEND ls_filetable TO lt_filetable.

    REFRESH: lt_return.

    CALL METHOD file_download
      EXPORTING
        it_filename    = lt_filetable
        iv_direct_down = 'X'
      RECEIVING
        rt_return      = lt_return
      EXCEPTIONS
        error          = 1
        OTHERS         = 2.

    CLEAR: lv_message.

    LOOP AT lt_return INTO ls_return WHERE type CA 'EAX'.
      lv_type    = 'E'.
      IF lv_message IS INITIAL.
        lv_message = ls_return-message.
      ELSE.
        lv_message = lv_message && ls_return-message.
      ENDIF.
    ENDLOOP.

    IF lv_type = 'E'.
      MESSAGE e000(zbc) WITH lv_message.
    ENDIF.
  ENDMETHOD.


  METHOD attachment_upload.
    DATA:lt_file      TYPE filetable,
         ls_file      TYPE file_table,
         lt_filetable TYPE zbc_tt_filetable,
         ls_filetable TYPE zbc_s_filetable,
         lt_return    TYPE zbc_tt_fupload_return,
         ls_return    TYPE zbc_s_fupload_return,
         lv_dockey    TYPE string,
         lv_error     TYPE char1,
         lv_lines     TYPE i.

    CALL METHOD file_open_dialog
      EXPORTING
        multiselection = iv_multiselection
      IMPORTING
        filetable      = lt_file.

    DESCRIBE TABLE lt_file LINES lv_lines.

    IF lt_file IS NOT INITIAL.
      CLEAR: ls_file .
      LOOP AT lt_file INTO ls_file.
        CLEAR: ls_filetable.
        ls_filetable-fullname = ls_file-filename.

        IF lv_lines > 1.
          ls_filetable-filename = iv_filename && sy-tabix.
        ELSE.
          ls_filetable-filename = iv_filename.
        ENDIF.

        CONDENSE ls_filetable-filename NO-GAPS.

        APPEND ls_filetable TO lt_filetable.
      ENDLOOP.

      CALL METHOD file_upload
        EXPORTING
          filetable = lt_filetable
        RECEIVING
          return    = lt_return
        EXCEPTIONS
          error     = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
      ELSE.
        CLEAR: lv_dockey,lv_error.
        LOOP AT lt_return INTO ls_return.
          IF ls_return-type CA 'EAX'.
            lv_error = 'X'.
          ELSE.
            IF lv_dockey IS INITIAL.
              lv_dockey = ls_return-dockey.
            ELSE.
              lv_dockey = lv_dockey && ',' && ls_return-dockey.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF lv_error IS NOT INITIAL.
          MESSAGE s000(zhr) WITH '文件上传失败' DISPLAY LIKE 'E'.
        ELSE.

          IF lv_dockey IS NOT INITIAL .
            cv_dockey = lv_dockey.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD build_fcat.

    DATA: ls_fcat TYPE lvc_s_fcat.
    col_pos = col_pos + 1.
    ls_fcat-col_pos     = col_pos.
    ls_fcat-fieldname   = iv_fieldname.
    ls_fcat-reptext     = iv_reptext.
    ls_fcat-ref_table   = iv_ref_table.
    ls_fcat-ref_field   = iv_ref_field.
    ls_fcat-key         = iv_key.
    ls_fcat-coltext     = iv_reptext.
    ls_fcat-scrtext_l   = iv_reptext.
    ls_fcat-scrtext_m   = iv_reptext.
    ls_fcat-scrtext_s   = iv_reptext.
    ls_fcat-no_zero     = iv_nozero.
    ls_fcat-do_sum      = iv_dosum.
    ls_fcat-convexit    = iv_convexit.
    ls_fcat-edit        = iv_edit.
    ls_fcat-checkbox    = iv_checkbox.
    ls_fcat-no_out      = iv_no_out.
    ls_fcat-hotspot     = iv_hotspot.
    APPEND ls_fcat TO ct_fcat.

  ENDMETHOD.


  METHOD constructor.

    "获取token的配置
    CLEAR: token_config.
    SELECT SINGLE *
      INTO token_config
      FROM zhrintbst001
     WHERE systy   = sy-sysid
       AND zzinttp = '10' .

    "文件上载的配置
    CLEAR: file_upload_config.
    SELECT SINGLE *
      INTO file_upload_config
      FROM zhrintbst001
     WHERE systy   = sy-sysid
       AND zzinttp = '50' .


    "文件下载的配置
    CLEAR: file_download_config.
    SELECT SINGLE *
      INTO file_download_config
      FROM zhrintbst001
     WHERE systy   = sy-sysid
       AND zzinttp = '60' .
  ENDMETHOD.


  METHOD directory_browse.
    DATA: lv_init_dir  TYPE string,
          lv_title     TYPE string,
          lt_filetable TYPE filetable,
          ls_filetable TYPE file_table,
          lv_rc        TYPE i,
          lv_dir_len type i.

    lv_init_dir = 'C:\'.

    lv_title = TEXT-002."选择路径

    CALL METHOD cl_gui_frontend_services=>directory_browse
      EXPORTING
        window_title         = lv_title
        initial_folder       = lv_init_dir
      CHANGING
        selected_folder      = dir
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
    ELSE.
      lv_dir_len = strlen( dir ).

      IF lv_dir_len > 1.
        lv_dir_len = lv_dir_len - 1.

        IF dir+lv_dir_len(1) <> '\'.
          dir = dir && '\'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD document_show.
    DATA: lt_return    TYPE zbc_tt_fdownload_return,
          ls_return    TYPE zbc_s_fdownload_return,
          lt_filetable TYPE zbc_tt_filetable,
          ls_filetable LIKE LINE OF lt_filetable,
          lt_data      TYPE TABLE OF zbc_s_fdownload_return-data,
          lv_size      TYPE i,
*          lv_url       TYPE c LENGTH 256,
          lv_tabix     TYPE sy-tabix.

    DATA: lt_html_data TYPE STANDARD TABLE OF x255,
          lv_url       TYPE char255,
          lv_type      TYPE text20,
          lv_subtype   TYPE text20,
          lv_app_type  TYPE string.

    CLEAR: lt_filetable,ls_filetable.
    ls_filetable-dockey   = iv_dockey.
    ls_filetable-filename = iv_filename.
    APPEND ls_filetable TO lt_filetable.

    lt_return = file_download( EXPORTING it_filename    = lt_filetable
                                         iv_direct_down = '' ).

    SORT lt_return BY dockey.
    READ TABLE lt_return INTO ls_return INDEX 1.
    IF sy-subrc EQ 0.

      IF ls_return-type = 'E'.
        MESSAGE s000 WITH ls_return-message DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      lv_app_type = get_application_type( EXPORTING
                                            iv_dockey  = iv_dockey
                                          IMPORTING
                                            ex_type    = lv_type
                                            ex_subtype = lv_subtype ).

      CASE lv_app_type.
        WHEN app_type_html.
          CALL METHOD html_show
            EXPORTING
              iv_data     = ls_return-data
              iv_type     = lv_type
              iv_subtype  = lv_subtype
              iv_filename = iv_filename.

        WHEN app_type_ole.
          CALL METHOD ole_show
            EXPORTING
              iv_data     = ls_return-data
              iv_type     = lv_type
              iv_subtype  = lv_subtype
              iv_filename = iv_filename.

        WHEN OTHERS.
          MESSAGE i000 WITH TEXT-008.
          RETURN.
      ENDCASE.


    ENDIF.

  ENDMETHOD.


  METHOD filelist_show.

    DATA: lt_filelist TYPE zbc_tt_filelist_return,
          ls_filelist TYPE zbc_s_filelist_return,
          ls_data     TYPE zbc_s_fdownload_return.

    DATA: ls_layout  TYPE lvc_s_layo,
          ls_variant TYPE disvariant,
          ls_fcat    TYPE lvc_s_fcat,
          lt_fcat    TYPE lvc_t_fcat.

    REFRESH: mt_filelist.
    LOOP AT it_data INTO ls_data.
      CLEAR: ls_filelist.

      MOVE-CORRESPONDING ls_data TO ls_filelist.

      ls_filelist-show = icon_display.

      APPEND ls_filelist TO mt_filelist.
    ENDLOOP.


    "初始化控件
    IF grid IS NOT INITIAL.
      grid->free( ).
      FREE grid.
    ENDIF.

    IF dialog_container IS NOT INITIAL.
      dialog_container->free( ).
      FREE dialog_container.
    ENDIF.

    CREATE OBJECT dialog_container
      EXPORTING
        width                       = 500
        height                      = 130
        top                         = 120
        left                        = 120
        caption                     = '附件清单'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        event_already_registered    = 6
        error_regist_event          = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    SET HANDLER handle_close FOR dialog_container.

    CREATE OBJECT grid
      EXPORTING
        i_parent          = dialog_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      SET HANDLER handle_hotspot_click FOR grid.
    ENDIF.

    ls_layout-zebra      = 'X'.
    ls_layout-cwidth_opt = 'X'.

    ls_variant-report = sy-repid.
    ls_variant-handle = 'FL'.

    REFRESH: lt_fcat.
    CLEAR: col_pos.
    CALL METHOD build_fcat
      EXPORTING
        iv_fieldname = 'SHOW'
        iv_reptext   = TEXT-c01
        iv_ref_table = 'ZBC_S_FILELIST_RETURN'
        iv_ref_field = 'SHOW'
        iv_key       = 'X'
        iv_hotspot   = 'X'
      CHANGING
        ct_fcat      = lt_fcat.

    CALL METHOD build_fcat
      EXPORTING
        iv_fieldname = 'FILEKEY'
        iv_reptext   = TEXT-c02
        iv_ref_table = 'ZBC_S_FILELIST_RETURN'
        iv_ref_field = 'FILEKEY'
      CHANGING
        ct_fcat      = lt_fcat.

    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        i_default                     = 'X'
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = mt_filelist
        it_fieldcatalog               = lt_fcat
*       it_sort                       =
*       it_filter                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.



  ENDMETHOD.


  METHOD file_display.
    DATA: lt_return   TYPE zbc_tt_fdownload_return,
          ls_return   TYPE zbc_s_fdownload_return,
          ls_filename TYPE zbc_s_filetable,
          lt_dockey   TYPE TABLE OF string,
          lv_dockey   TYPE string,
          lv_lines    TYPE i.

    REFRESH: lt_return.

    LOOP AT it_filename INTO ls_filename.

      CLEAR: ls_return.

      MOVE-CORRESPONDING ls_filename TO ls_return.

      REFRESH: lt_dockey.

      SPLIT ls_filename-dockey AT ',' INTO TABLE lt_dockey.

      LOOP AT lt_dockey INTO lv_dockey.

        ls_return-filekey  = lv_dockey.
        APPEND ls_return TO lt_return.
      ENDLOOP.
    ENDLOOP.

    DESCRIBE TABLE lt_return LINES lv_lines.

    IF lv_lines > 1.
      "show dialog
      CALL METHOD filelist_show
        EXPORTING
          it_data = lt_return.
    ELSEIF lv_lines EQ 1.
      "show document
      READ TABLE lt_return INTO ls_return INDEX 1.
      IF sy-subrc EQ 0.
        CALL METHOD document_show
          EXPORTING
            iv_dockey   = ls_return-filekey
            iv_filename = ls_return-filename.
      ENDIF.
    ELSE.
      RETURN.
    ENDIF.



  ENDMETHOD.


  METHOD file_download.

    DATA: lob_http_client        TYPE REF TO if_http_client,
          lob_multipart          TYPE REF TO if_http_entity,
          lob_multipart1         TYPE REF TO if_http_entity,
          lv_content_disposition TYPE string,
          lt_content_type        TYPE TABLE OF string,
          lv_content_type        TYPE string,
          lt_string              TYPE TABLE OF string,
          lv_string              TYPE string,
          lv_str_len             TYPE i,
          lv_url                 TYPE string,
          lt_dockey              TYPE TABLE OF string,
          lv_dockey              TYPE string,
          lv_dockey_lines        TYPE i,
          lv_author              TYPE string,
          lv_result              TYPE xstring,
          lv_filename            TYPE string,
          lv_file_seq            TYPE char10,
          lt_extension           TYPE TABLE OF string,
          lv_extension           TYPE string,
          lv_lines               TYPE i,
          ls_return              TYPE zbc_s_fdownload_return,
          lv_token_flag          TYPE char1,
          lv_dir                 TYPE string,
          lv_file_count          TYPE i,
          lv_percentage          TYPE i,
          lv_text                TYPE string.


    IF file_download_config IS INITIAL.
      MESSAGE e002 WITH '文件下载' RAISING error .
      RETURN.
    ENDIF.

    "选择路径
    IF iv_direct_down IS NOT INITIAL.
      lv_dir = directory_browse( ).
      IF lv_dir IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    "文件数量
    DESCRIBE TABLE it_filename LINES lv_file_count .

    LOOP AT it_filename INTO file_table.

      CLEAR: ls_return.
      MOVE-CORRESPONDING file_table TO ls_return .

      "消息提示
      CLEAR: lv_percentage .
      lv_percentage = sy-tabix / lv_file_count * 100.

      lv_text  = TEXT-005."正在下载文件
      REPLACE '&1' IN lv_text WITH file_table-dockey.

      CALL METHOD progress_indicator
        EXPORTING
          percentage = lv_percentage
          text       = lv_text.

      "获取Token
      CLEAR: lv_token_flag.
      DO 3 TIMES.
        CALL METHOD get_token
          EXCEPTIONS
            error  = 1
            OTHERS = 2.
        IF sy-subrc EQ 0.
          lv_token_flag = 'X'.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.

      IF lv_token_flag IS INITIAL.
        ls_return-type = 'E'.
        MESSAGE e001 INTO ls_return-message.
      ELSE.

        REFRESH: lt_dockey.

        SPLIT file_table-dockey AT ',' INTO TABLE lt_dockey.

        DESCRIBE TABLE lt_dockey LINES lv_dockey_lines.

        LOOP AT lt_dockey INTO lv_dockey.
          "多个附件时，下载文件增加序号最后缀
          CLEAR: lv_file_seq .
          IF lv_dockey_lines > 1.
            lv_file_seq =  sy-tabix.
          ENDIF.

          CALL METHOD zcl_attachment=>progress_indicator
            EXPORTING
              text = TEXT-009.

          ls_return-filekey = lv_dockey. "单个文件的URL

          CLEAR: lv_url.
          lv_url = file_download_config-host && lv_dockey && '&jti=' && token-jti.

          CALL METHOD cl_http_client=>create_by_url
            EXPORTING
              url                = lv_url
            IMPORTING
              client             = lob_http_client
            EXCEPTIONS
              argument_not_found = 1
              plugin_not_active  = 2
              internal_error     = 3
              OTHERS             = 4.
          IF sy-subrc EQ 0.

            "请求方法
            CALL METHOD lob_http_client->request->set_header_field
              EXPORTING
                name  = '~request_method'
                value = 'GET'.
            "请求发送
            lob_http_client->send(
                 EXCEPTIONS
                   http_communication_failure = 1
                   http_invalid_state         = 2 ).

            "请求接收
            lob_http_client->receive(
              EXCEPTIONS
                http_communication_failure = 1
                http_invalid_state         = 2
                http_processing_failed     = 3 ).

            "获取结果
            CLEAR: lv_result.
            lv_result = lob_http_client->response->get_data( ).

            "获取文件信息
            CLEAR: lv_content_disposition.
            CALL METHOD lob_http_client->response->get_header_field
              EXPORTING
                name  = 'content-disposition'
              RECEIVING
                value = lv_content_disposition.

            "获取文件类型
            CLEAR: lv_content_type.
            CALL METHOD lob_http_client->response->get_header_field
              EXPORTING
                name  = 'Content-Type'
              RECEIVING
                value = lv_content_type.
            ls_return-contenttype = lv_content_type.

            SPLIT lv_content_type  AT ';' INTO TABLE lt_content_type .
            READ TABLE lt_content_type INTO lv_content_type INDEX 1.
            IF sy-subrc EQ 0.
              CONDENSE lv_content_type.

              SPLIT  lv_content_type AT '/' INTO  ls_return-application_key ls_return-application .
            ENDIF.

            "提取文件名和文件名后缀
            CLEAR lv_filename.
            SPLIT lv_content_disposition AT ';' INTO TABLE lt_string.
            LOOP AT lt_string INTO lv_string.
              CONDENSE lv_string.
              lv_str_len = strlen( lv_string ).
              IF lv_str_len > 8.
                IF lv_string+0(8) = 'filename'.
                  lv_filename = lv_string+9.
                  REPLACE ALL OCCURRENCES OF '"' IN lv_filename WITH space.
                  CONDENSE lv_filename .

                  "如果自定义文件名不为空
                  IF file_table-filename IS NOT INITIAL.

                    REFRESH: lt_extension .
                    SPLIT lv_filename AT '.' INTO TABLE lt_extension .

                    CLEAR: lv_lines.
                    DESCRIBE TABLE lt_extension LINES lv_lines.
                    "文件名增加序号
                    CLEAR: lv_filename.
                    IF lv_file_seq IS NOT INITIAL.
                      lv_filename = file_table-filename && '-' && lv_file_seq.
                      CONDENSE lv_filename NO-GAPS.
                    ELSE.
                      lv_filename = file_table-filename.
                    ENDIF.

                    CLEAR: lv_extension .
                    IF lv_lines > 0.
                      READ TABLE lt_extension INTO lv_extension INDEX lv_lines.
                      lv_filename = lv_filename && '.' && lv_extension.
                    ENDIF.
                  ENDIF.

                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.

            "获取状态
            lob_http_client->response->get_status( IMPORTING code   = DATA(lv_code)
                                                             reason = DATA(lv_reason) ).

            ls_return-code     = lv_code.
            ls_return-reason   = lv_reason.
            ls_return-data     = lv_result.

            IF lv_code = 200.
              "下载文件到本地
              IF iv_direct_down IS NOT INITIAL.

                lv_filename        = lv_dir && lv_filename .
                ls_return-filename = lv_filename.

                CALL METHOD gui_download
                  EXPORTING
                    filename = lv_filename
                    data     = lv_result
                  EXCEPTIONS
                    error    = 1
                    OTHERS   = 2.
                IF sy-subrc <> 0.
                  ls_return-type = 'E'.
                  MESSAGE ID sy-msgid
                        TYPE sy-msgty
                      NUMBER sy-msgno
                        WITH sy-msgv1
                             sy-msgv2
                             sy-msgv3
                             sy-msgv4
                        INTO ls_return-message.
                ELSE.
                  ls_return-type = 'S'.
                  ls_return-message = TEXT-004."文件下载成功
                ENDIF.
              ELSE.
                ls_return-type = 'S'.
                ls_return-message = TEXT-004."文件下载成功
              ENDIF.
            ELSE.
              ls_return-type = 'E'.
              ls_return-message = ls_return-code && ls_return-reason && TEXT-003."从文档服务器下载文件失败
            ENDIF.
          ENDIF.

          APPEND ls_return TO rt_return.
        ENDLOOP.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD file_open_dialog.
    DATA: lv_init_dir  TYPE string,
          lv_title     TYPE string,
          lt_filetable TYPE filetable,
          ls_filetable TYPE file_table,
          lv_rc        TYPE i,
          lv_user_action  TYPE i.

    lv_init_dir = 'C:\'.
    lv_title = TEXT-001."选择文件
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = lv_title
        initial_directory       = lv_init_dir
        multiselection          = multiselection
      CHANGING
        file_table              = filetable
        rc                      = lv_rc
        user_action             = lv_user_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.

    ELSE.

    ENDIF.

  ENDMETHOD.


  METHOD file_save_dialog.
    DATA: lv_init_dir  TYPE string,
          lv_title     TYPE string,
          lt_filetable TYPE filetable,
          ls_filetable TYPE file_table,
          lv_rc        TYPE i.

    lv_init_dir = 'C:\'.

    CLEAR: filename,
           filepath,
           fullname.

    lv_title = TEXT-002."选择路径

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title              = lv_title
        initial_directory         = lv_init_dir
        default_file_name         = init_filename
      CHANGING
        filename                  = filename
        path                      = filepath
        fullpath                  = fullname
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.



  ENDMETHOD.


  METHOD file_upload.
    DATA: lob_http_client        TYPE REF TO if_http_client,
          lob_multipart          TYPE REF TO if_http_entity,
          lv_content_disposition TYPE string,
          lv_data                TYPE xstring,
          lv_url                 TYPE string,
          lv_author              TYPE string,
          lv_result              TYPE string,
          ls_return              TYPE zbc_s_fupload_return,
          lv_token_flag          TYPE char1,
          lt_extension           TYPE TABLE OF string,
          lv_extension           TYPE string,
          lv_filename            TYPE string,
          lv_lines               TYPE i,
          lv_file_count          TYPE i,
          lv_percentage          TYPE i,
          lv_text                TYPE string.

    IF file_upload_config IS INITIAL.
      MESSAGE e002 WITH '文件下载' RAISING error .
      RETURN.
    ENDIF.

    "文件数量
    DESCRIBE TABLE filetable LINES lv_file_count .

    LOOP AT filetable INTO file_table.
      CLEAR: ls_return.
      ls_return-fullname = file_table-fullname.

      "消息提示
      CLEAR: lv_percentage .
      lv_percentage = sy-tabix / lv_file_count * 100.

      lv_text  = TEXT-006.
      REPLACE '&1' IN lv_text WITH file_table-fullname.

      CALL METHOD progress_indicator
        EXPORTING
          percentage = lv_percentage
          text       = lv_text.

      "获取Token
      CLEAR: lv_token_flag.
      DO 3 TIMES.
        CALL METHOD get_token
          EXCEPTIONS
            error  = 1
            OTHERS = 2.
        IF sy-subrc EQ 0.
          lv_token_flag = 'X'.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.

      IF lv_token_flag IS INITIAL.
        MESSAGE e001 RAISING error.
        EXIT.
      ENDIF.

      "拆分文件名
      CLEAR: fullname,
             filename,
             filepath.
      fullname = file_table-fullname.

      CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
        EXPORTING
          full_name     = fullname
        IMPORTING
          stripped_name = filename
          file_path     = filepath
        EXCEPTIONS
          x_error       = 1
          OTHERS        = 2.

      IF file_table-filename IS NOT INITIAL.
        "如果自定义文件名不为空
        IF file_table-filename IS NOT INITIAL.
          lv_filename = filename.
          REFRESH: lt_extension .
          SPLIT lv_filename AT '.' INTO TABLE lt_extension .

          CLEAR: lv_lines.
          DESCRIBE TABLE lt_extension LINES lv_lines.

          CLEAR: lv_extension .
          IF lv_lines > 0.
            READ TABLE lt_extension INTO lv_extension INDEX lv_lines.
            lv_filename = file_table-filename && '.' && lv_extension.
          ENDIF.
        ENDIF.

        filename = lv_filename.
      ENDIF.

      "文件上载
      CLEAR: lv_data.
      CALL METHOD gui_upload
        EXPORTING
          filename = fullname
        RECEIVING
          data     = lv_data
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
      ENDIF.

      "创建Client
      lv_url = file_upload_config-host.

      cl_http_client=>create_by_url(
           EXPORTING
             url                = lv_url
           IMPORTING
             client             = lob_http_client
           EXCEPTIONS
             argument_not_found = 1
             plugin_not_active  = 2
             internal_error     = 3
             OTHERS             = 4 ).

      IF sy-subrc EQ 0.


        CALL METHOD lob_http_client->request->set_header_field
          EXPORTING
            name  = 'Content-Type'
            value = 'multipart/form-data;charset=utf-8'.

        lv_author = file_upload_config-author.

        "授权
        CALL METHOD lob_http_client->request->set_header_field
          EXPORTING
            name  = 'Authorization'
            value = lv_author.

        "token
        CALL METHOD lob_http_client->request->set_header_field
          EXPORTING
            name  = token-token_key
            value = token-token.

        "Create Multipart,创建文件属性
        lob_multipart = lob_http_client->request->if_http_entity~add_multipart( ).
        lv_content_disposition = `form-data;name="file";filename=` && filename &&
                                 `;content-type="text/plain";charset=utf-8`.
        "
        CALL METHOD lob_multipart->set_header_field
          EXPORTING
            name  = 'content-disposition'
            value = lv_content_disposition.

        CALL METHOD lob_multipart->set_content_type
          EXPORTING
            content_type = 'multipart/form-data;charset=utf-8'.

        "Sets the HTTP body of this entity to the given binary data
        CALL METHOD lob_multipart->set_data
          EXPORTING
            data = lv_data.

        "请求方法
        CALL METHOD lob_http_client->request->set_header_field
          EXPORTING
            name  = '~request_method'
            value = 'POST'.
        "请求发送
        lob_http_client->send(
             EXCEPTIONS
               http_communication_failure = 1
               http_invalid_state         = 2 ).
        "请求接收
        lob_http_client->receive(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3 ).
        "获取结果
        CLEAR: lv_result.
        lv_result = lob_http_client->response->get_cdata( ).

        "获取状态
        lob_http_client->response->get_status( IMPORTING code   = DATA(lv_code)
                                                         reason = DATA(lv_reason) ).

        ls_return-code     = lv_code.
        ls_return-reason   = lv_reason.

        CALL METHOD /ui2/cl_json=>deserialize
          EXPORTING
            json             = lv_result
            assoc_arrays     = 'X'
            assoc_arrays_opt = 'X'
          CHANGING
            data             = ls_return.
        IF ls_return-code = 200.
          ls_return-type    = 'S'.
          ls_return-message = ls_return-msg.
          ls_return-dockey  = ls_return-data.
          IF ls_return-dockey is INITIAL.
          ls_return-type    = 'E'.
          ls_return-message = text-007."文档服务器未返回文件名
          ENDIF.
        ELSE.
          ls_return-type    = 'E'.
          ls_return-message = ls_return-msg.
        ENDIF.
      ENDIF.

      APPEND ls_return TO return.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_application_type.
    DATA:
      lt_extension TYPE TABLE OF string,
      lv_extension TYPE string,
      lv_lines     TYPE i,
      lt_html_data TYPE STANDARD TABLE OF x255,
      lv_url       TYPE char255.


    "拆分文件名
    CLEAR: lt_extension,lv_extension .
    SPLIT iv_dockey AT '.' INTO TABLE lt_extension .

    CLEAR: lv_lines.
    DESCRIBE TABLE lt_extension LINES lv_lines.

    CLEAR: lv_extension .
    IF lv_lines > 0.
      READ TABLE lt_extension INTO lv_extension INDEX lv_lines.
    ENDIF.

    CONDENSE lv_extension NO-GAPS.
    TRANSLATE lv_extension TO UPPER CASE.

    "文件名格式
    CLEAR: ex_type   ,
           ex_subtype.
    CASE lv_extension.
      WHEN filetype_bmp OR
           filetype_gif OR
           filetype_jpg OR
           filetype_png OR
           filetype_tif  .

        ex_type    = 'image'.
        ex_subtype = lv_extension.
        rt_app_type = app_type_html.

      WHEN filetype_pdf.
        ex_type    = 'application'.
        ex_subtype = 'pdf'.
        rt_app_type = app_type_html.

      WHEN filetype_txt OR filetype_csv.
        ex_type    = 'BIN'.
        ex_subtype = 'txt'.
        rt_app_type = app_type_html.

      WHEN filetype_doc OR filetype_docx.
        ex_type    = 'R/3 Basis'.
        ex_subtype = 'Word.Document'.
        rt_app_type = app_type_ole.

      WHEN filetype_xls OR filetype_xlsx.
        ex_type    = 'R/3 Basis'.
        ex_subtype = 'Excel.Sheet'.
        rt_app_type = app_type_ole.
      WHEN filetype_ppt OR filetype_pptx.
        ex_type    = 'R/3 Basis'.
        ex_subtype = 'PowerPoint.Slide'.
        rt_app_type = app_type_ole.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD get_token.
    DATA: lob_http_client TYPE REF TO if_http_client,
          lv_url          TYPE string,
          lv_author       TYPE string,
          lv_result       TYPE string,
          ls_token        TYPE zhrintbss001.

    IF token_config IS INITIAL.
      MESSAGE e002 WITH 'Token' RAISING error .
      RETURN.
    ENDIF.

    lv_url = token_config-host.



    CONCATENATE lv_url '?&tenantId=000000'
                '&username=' token_config-userid
                '&password=' token_config-password
                '&grant_type=password'
                '&scope=all'
                '&type=account'
                INTO lv_url.


    cl_http_client=>create_by_url(
         EXPORTING
           url                = lv_url
         IMPORTING
           client             = lob_http_client
         EXCEPTIONS
           argument_not_found = 1
           plugin_not_active  = 2
           internal_error     = 3
           OTHERS             = 4 ).
    IF sy-subrc EQ 0.

      CALL METHOD lob_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'application/x-www-form-urlencoded'.


      "授权
      lv_author = token_config-author.
      CALL METHOD lob_http_client->request->set_header_field
        EXPORTING
          name  = 'Authorization'
          value = lv_author.
      "请求方法
      CALL METHOD lob_http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'POST'.

      "发送HTTP
      lob_http_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2 ).

      "接收请求
      lob_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3 ).
      "获取返回结果
      lv_result = lob_http_client->response->get_cdata( ).

      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json        = lv_result
          pretty_name = 'X'
        CHANGING
          data        = ls_token.

      lob_http_client->response->get_status( IMPORTING code   = DATA(lv_code)
                                                       reason = DATA(lv_reason) ).

      IF lv_code = 200.
        CLEAR: token.

        token-author_key = c_author_key.
        token-author     = lv_author.
        token-token_key  = c_token_key.
        CONCATENATE ls_token-token_type
                    ls_token-access_token
               INTO token-token SEPARATED BY space      .
        token-jti_key = c_jti_key.
        token-jti     = ls_token-jti.

        rv_token = token.
      ELSE.
        MESSAGE e001 RAISING error.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD gui_download.
    DATA: lv_filelength TYPE i,
          lt_data       TYPE TABLE OF x.

    TRY.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = data
          IMPORTING
            output_length = lv_filelength
          TABLES
            binary_tab    = lt_data.

        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = filename
            filetype                = 'BIN'
          CHANGING
            data_tab                = lt_data
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            not_supported_by_gui    = 22
            error_no_gui            = 23
            OTHERS                  = 24.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid
                TYPE sy-msgty
              NUMBER sy-msgno
                WITH sy-msgv1
                     sy-msgv2
                     sy-msgv3
                     sy-msgv4
             RAISING error.
        ENDIF.
      CATCH cx_root INTO DATA(lox_root).
        MESSAGE e000 WITH lox_root->if_message~get_text( ) RAISING error.
    ENDTRY.
  ENDMETHOD.


  METHOD gui_upload.
    DATA: lv_filelength TYPE i,
          lt_data       TYPE TABLE OF x.

    TRY.
        CALL METHOD cl_gui_frontend_services=>gui_upload
          EXPORTING
            filename                = filename
            filetype                = 'BIN'
*           has_field_separator     = SPACE
*           header_length           = 0
*           read_by_line            = 'X'
*           dat_mode                = SPACE
*           codepage                = SPACE
*           ignore_cerr             = ABAP_TRUE
*           replacement             = '#'
*           virus_scan_profile      =
          IMPORTING
            filelength              = lv_filelength
*           header                  =
          CHANGING
            data_tab                = lt_data[]
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19.


        IF sy-subrc EQ 0.
          CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
            EXPORTING
              input_length = lv_filelength
            IMPORTING
              buffer       = data
            TABLES
              binary_tab   = lt_data[]
            EXCEPTIONS
              failed       = 1
              OTHERS       = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid
                  TYPE sy-msgty
                NUMBER sy-msgno
                  WITH sy-msgv1
                       sy-msgv2
                       sy-msgv3
                       sy-msgv4
               RAISING error.
          ENDIF.

        ELSE.
          MESSAGE ID sy-msgid
                TYPE sy-msgty
              NUMBER sy-msgno
                WITH sy-msgv1
                     sy-msgv2
                     sy-msgv3
                     sy-msgv4
             RAISING error.
        ENDIF.
      CATCH cx_root INTO DATA(lox_root).
        MESSAGE e000 WITH lox_root->if_message~get_text( ) RAISING error.
    ENDTRY.
  ENDMETHOD.


  METHOD handle_close.
    CALL METHOD sender->set_visible
      EXPORTING
        visible = space.
  ENDMETHOD.


  METHOD handle_hotspot_click.

    DATA: lt_outtab   TYPE REF TO data,
          ls_filelist TYPE zbc_s_filelist_return,
          lv_filename TYPE string,
          lv_tabix    TYPE sy-tabix.

*    FIELD-SYMBOLS: <lfs_outtab> TYPE STANDARD TABLE.
*
*    lt_outtab = grid->get_outtab( ).
*
*    ASSIGN lt_outtab->* TO <lfs_outtab>.

    READ TABLE mt_filelist INTO ls_filelist INDEX e_row_id-index.
    IF sy-subrc EQ 0.
      lv_tabix    = e_row_id-index.
      lv_filename = ls_filelist-filename && '-' && lv_tabix.
      CONDENSE lv_filename NO-GAPS.

      CALL METHOD document_show
        EXPORTING
          iv_dockey   = ls_filelist-filekey
          iv_filename = lv_filename.
    ENDIF.

  ENDMETHOD.


  method HANDLE_TOOLBAR.

  endmethod.


  METHOD html_show.
    DATA: lt_html_data TYPE STANDARD TABLE OF x255,
          lv_url       TYPE char255,
          lv_size      TYPE i,
          lv_caption   TYPE char30.

    CALL METHOD zcl_attachment=>progress_indicator
      EXPORTING
        text = TEXT-010.

    lv_caption = iv_filename.

    IF iv_data IS NOT INITIAL.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = iv_data
        IMPORTING
          output_length = lv_size
        TABLES
          binary_tab    = lt_html_data.
    ENDIF.

    "初始化控件
    IF html_viewer IS NOT INITIAL.
      html_viewer->free( ).
      FREE html_viewer.
    ENDIF.

    IF docu_container IS NOT INITIAL.
      docu_container->free( ).
      FREE docu_container.
    ENDIF.

    CREATE OBJECT docu_container
      EXPORTING
        width                       = 1000
        height                      = 300
        top                         = 120
        left                        = 50
        caption                     = lv_caption
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        event_already_registered    = 6
        error_regist_event          = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET HANDLER handle_close FOR docu_container.

    CREATE OBJECT html_viewer
      EXPORTING
        parent             = docu_container
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "加载数据
    CALL METHOD html_viewer->load_data
      EXPORTING
        type                   = iv_type    "'application'
        subtype                = iv_subtype "'pdf'
        size                   = lv_size
      IMPORTING
        assigned_url           = lv_url
      CHANGING
        data_table             = lt_html_data
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
    ENDIF.

    "显示
    CALL METHOD html_viewer->show_url
      EXPORTING
        url                    = lv_url
        in_place               = ' X'
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.

    ENDIF.


  ENDMETHOD.


  METHOD ole_show.
    DATA : lv_app_name       TYPE char3,
           lv_doc_type       TYPE text20,
           ole               TYPE REF TO i_oi_container_control,
           lo_doc_proxy      TYPE REF TO i_oi_document_proxy,
           lo_error          TYPE REF TO i_oi_error,
           ls_retcode        TYPE soi_ret_string,
           lv_document_title TYPE sdbah-actid,
           lv_caption        TYPE char30.

    lv_caption = iv_filename.

    DATA: lt_html_data TYPE STANDARD TABLE OF x255,
          lv_url       TYPE char255,
          lv_size      TYPE i.

    CALL METHOD zcl_attachment=>progress_indicator
      EXPORTING
        text = TEXT-010.

    lv_app_name = iv_type.
    lv_doc_type = iv_subtype.


    IF iv_data IS NOT INITIAL.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = iv_data
        IMPORTING
          output_length = lv_size
        TABLES
          binary_tab    = lt_html_data.
    ENDIF.

    "初始化控件
    IF html_viewer IS NOT INITIAL.
      html_viewer->free( ).
      FREE html_viewer.
    ENDIF.

    IF docu_container IS NOT INITIAL.
      docu_container->free( ).
      FREE docu_container.
    ENDIF.

    CREATE OBJECT docu_container
      EXPORTING
        width                       = 1000
        height                      = 300
        top                         = 120
        left                        = 50
        caption                     = lv_caption
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        event_already_registered    = 6
        error_regist_event          = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET HANDLER handle_close FOR docu_container.

*    DATA:
*      document_viewer  TYPE REF TO i_oi_document_viewer.
*
*    CALL METHOD c_oi_container_control_creator=>get_document_viewer
*      IMPORTING
*        viewer = document_viewer.
*
*    CALL METHOD document_viewer->init_viewer
*      EXPORTING
*        parent = docu_container.
**
**    CALL FUNCTION 'DP_CREATE_URL'
**      EXPORTING
**        type    = 'application'
**        subtype = 'x-oleobject'
**        size    = lv_size
**      TABLES
**        data    = lt_html_data
**      CHANGING
**        url     = lv_url.
*
**    CALL METHOD document_viewer->view_document_from_url
**      EXPORTING
**        document_url = lv_url
**        show_inplace = 'X'.
*
*    CALL METHOD document_viewer->view_document_from_table
*      EXPORTING
*        show_inplace         = 'X'
*        type                 = 'application'
*        subtype              = 'x-oleobject'
*        size                 = lv_size
*      CHANGING
*        document_table       = lt_html_data
*      EXCEPTIONS
*        dp_invalid_parameter = 1
*        dp_error_general     = 2
*        cntl_error           = 3
*        not_initialized      = 4
*        invalid_parameter    = 5
*        OTHERS               = 6.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.


    c_oi_container_control_creator=>get_container_control(
                                     IMPORTING
                                       control = ole
                                       retcode = ls_retcode ).

    c_oi_errors=>raise_message( 'E' ).

    CALL METHOD ole->init_control
      EXPORTING
        r3_application_name      = lv_app_name
        inplace_enabled          = 'X'
        inplace_scroll_documents = 'X'
        parent                   = docu_container
        register_on_close_event  = 'X'
        register_on_custom_event = 'X'
      IMPORTING
        retcode                  = ls_retcode.

    c_oi_errors=>raise_message( 'E' ).

    ole->get_document_proxy(
                  EXPORTING document_type   = lv_doc_type
*                            document_format = 'OLE'
                  IMPORTING document_proxy = lo_doc_proxy
                            retcode = ls_retcode ).

    IF ls_retcode NE c_oi_errors=>ret_ok.
      EXIT.
    ENDIF.

    lv_document_title = iv_filename.

* This is working
    CALL METHOD lo_doc_proxy->open_document_from_table
      EXPORTING
        document_size    = lv_size
        document_table   = lt_html_data
        document_title   = lv_document_title
        open_inplace     = 'X'
        open_readonly    = 'X'
        protect_document = 'X'.

  ENDMETHOD.


  METHOD progress_indicator.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage
        text       = text.

  ENDMETHOD.
ENDCLASS.
