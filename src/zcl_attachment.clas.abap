class ZCL_ATTACHMENT definition
  public
  final
  create public .

public section.

  data TOKEN type ZBC_S_TOKEN .

  methods DIRECTORY_BROWSE
    returning
      value(DIR) type STRING .
  methods PROGRESS_INDICATOR
    importing
      !PERCENTAGE type I default 0
      !TEXT type STRING .
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
protected section.
private section.

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

    CLEAR: filename,
           filepath,
           fullname.

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


  METHOD file_download.

    DATA: lob_http_client        TYPE REF TO if_http_client,
          lob_multipart          TYPE REF TO if_http_entity,
          lob_multipart1         TYPE REF TO if_http_entity,
          lv_content_disposition TYPE string,
          lt_content_type        TYPE TABLE OF string,
          lv_content_type        TYPE string,
          lt_string              TYPE TABLE OF string,
          lv_string              TYPE string,
          lv_url                 TYPE string,
          lv_author              TYPE string,
          lv_result              TYPE xstring,
          lv_filename            TYPE string,
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

      lv_text  = TEXT-005.
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

        CLEAR: lv_url.
        lv_url = file_download_config-host && file_table-dockey && '&jti=' && token-jti.

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
          LOOP AT lt_content_type INTO lv_content_type.
            CONDENSE lv_content_type.
            IF lv_content_type+0(11) = 'application'.
              ls_return-application_key  = 'application'.
              ls_return-application      = lv_content_type+11.
            ENDIF.
          ENDLOOP.

          "提取文件名和文件名后缀
          CLEAR lv_filename.
          SPLIT lv_content_disposition AT ';' INTO TABLE lt_string.
          LOOP AT lt_string INTO lv_string.
            CONDENSE lv_string.
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

                CLEAR: lv_extension .
                IF lv_lines > 0.
                  READ TABLE lt_extension INTO lv_extension INDEX lv_lines.
                  lv_filename = file_table-filename && '.' && lv_extension.
                ENDIF.
              ENDIF.

              EXIT.
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
            ls_return-message = TEXT-003."从文档服务器下载文件失败
          ENDIF.

        ENDIF.
      ENDIF.

      APPEND ls_return TO rt_return.
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
          ls_return-dockey  = ls_return-data.
          ls_return-type    = 'S'.
          ls_return-message = ls_return-msg.
        ELSE.
          ls_return-type    = 'E'.
          ls_return-message = ls_return-msg.
        ENDIF.
      ENDIF.

      APPEND ls_return TO return.
    ENDLOOP.

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


  METHOD progress_indicator.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage
        text       = text.

  ENDMETHOD.
ENDCLASS.
