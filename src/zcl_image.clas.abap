class ZCL_IMAGE definition
  public
  create public .

public section.

  class-data C_TYPE_APPL type CHAR20 value 'APPL' ##NO_TEXT.
  constants C_TYPE_LOCAL type CHAR20 value 'LOCAL' ##NO_TEXT.
  constants C_TYPE_DOCSERVER type CHAR20 value 'DOC_SERVER' ##NO_TEXT.
  constants FORMAT_BMP type STRING value 'image/x-ms-bmp' ##NO_TEXT.
  constants FORMAT_TIF type STRING value 'image/tiff' ##NO_TEXT.
  constants FORMAT_JPG type STRING value 'image/jpeg' ##NO_TEXT.
  constants FORMAT_PNG type STRING value 'image/png' ##NO_TEXT.
  constants FORMAT_GIF type STRING value 'image/gif' ##NO_TEXT.

  class-methods GET_IMAGE
    importing
      !IMAGENAME type TDOBNAME
    exporting
      !EXIST type CHAR1
      !CONTENT type SBDST_CONTENT
      !COMPONENTS type SBDST_COMPONENTS
      !SIGNATURE type SBDST_SIGNATURE .
  methods UPLOAD_IMAGE
    importing
      !IV_FILENAME type STRING
      !IV_IMAGENAME type TDOBNAME default 'HRPHOTO'
      !IV_TYPE type CHAR20 optional
    exceptions
      ERROR .
  PROTECTED SECTION.
private section.

  constants CLASSNAME type SBDST_CLASSNAME value 'DEVC_STXD_BITMAP' ##NO_TEXT.
  constants CLASSTYPE type SBDST_CLASSTYPE value 'OT' ##NO_TEXT.

  methods GET_IMAGE_FROM_DOCSERVER
    importing
      !URL type STRING
    exporting
      !DATA type XSTRING
    exceptions
      ERROR .
  methods GET_IMAGE_FROM_APPL
    importing
      !FILENAME type STRING
    exporting
      !DATA type XSTRING
    exceptions
      ERROR .
  methods GET_IMAGE_FROM_LOCAL
    importing
      !FILENAME type STRING
    exporting
      !DATA type XSTRING
    exceptions
      ERROR .
  methods CONVERT_BITMAP_TO_BDS
    importing
      !IT_BITMAP_FILE type W3MIMETABTYPE
      !IV_BITMAP_BYTECOUNT type I
      !IV_IMAGENAME type TDOBNAME default 'HRPHOTO'
    exceptions
      ERROR .
  class-methods TABLE_TO_XSTRING
    importing
      !DATA_TABLE type STANDARD TABLE
    exporting
      value(DATA_STRING) type XSTRING .
ENDCLASS.



CLASS ZCL_IMAGE IMPLEMENTATION.


  METHOD convert_bitmap_to_bds.

    DATA: ls_stxbitmaps TYPE stxbitmaps,
          lv_tabname    TYPE bds_locl-tabname,
          lv_docid      TYPE stxbitmaps-docid,
          lv_widthpix   TYPE stxbitmaps-widthpix,
          lv_heightpix  TYPE stxbitmaps-heightpix,
          lv_widthtw    TYPE stxbitmaps-widthtw,
          lv_heighttw   TYPE stxbitmaps-heighttw,
          lv_resolution TYPE stxbitmaps-resolution.

    DATA:
*          lv_classname  TYPE sbdst_classname,
*          lv_classtype  TYPE sbdst_classtype,
      lt_components TYPE sbdst_components,
      ls_components TYPE bapicompon,
      lt_content    TYPE sbdst_content,
      lv_object_key TYPE sbdst_object_key,
      lt_signature  TYPE sbdst_signature,
      ls_signature  TYPE bapisignat.

    DATA: lt_bitmap_bds TYPE sbdst_content,
          lob_bds       TYPE REF TO cl_bds_document_set.


    CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP_BDS'
      EXPORTING
        color                     = 'X'
        format                    = 'BMP'
        resident                  = ' '
        bitmap_bytecount          = iv_bitmap_bytecount
        compress_bitmap           = ' '
      IMPORTING
        width_tw                  = lv_widthtw
        height_tw                 = lv_heighttw
        width_pix                 = lv_widthpix
        height_pix                = lv_heightpix
        dpi                       = lv_resolution
*       BDS_BYTECOUNT             =
      TABLES
        bitmap_file               = it_bitmap_file
        bitmap_file_bds           = lt_bitmap_bds
      EXCEPTIONS
        format_not_supported      = 1
        no_bmp_file               = 2
        bmperr_invalid_format     = 3
        bmperr_no_colortable      = 4
        bmperr_unsup_compression  = 5
        bmperr_corrupt_rle_data   = 6
        tifferr_invalid_format    = 7
        tifferr_no_colortable     = 8
        tifferr_unsup_compression = 9
        bmperr_eof                = 10
        OTHERS                    = 11.
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


    lv_object_key = iv_imagename.

    "检查图片是否存在

    REFRESH: lt_content,
             lt_signature,
             lt_components.
    CALL METHOD zcl_image=>get_image
      EXPORTING
        imagename  = iv_imagename
      IMPORTING
*       exist      =
        content    = lt_content
        components = lt_components
        signature  = lt_signature.
    IF lt_signature[] IS NOT INITIAL.
      READ TABLE lt_signature INTO ls_signature INDEX 1.
      IF sy-subrc EQ 0.
        lv_docid = ls_signature-doc_id.
      ENDIF.
    ENDIF.

    CLEAR: ls_components.
    ls_components-doc_count  = 1.
    ls_components-comp_count = 1.
    ls_components-mimetype   = format_bmp.
    ls_components-comp_id    = iv_imagename && '.bmp'.
    ls_components-comp_size  = iv_bitmap_bytecount.
    APPEND ls_components TO lt_components.

    CLEAR: ls_signature.
    ls_signature-doc_count = 1.
    APPEND ls_signature TO lt_signature.

    IF lv_docid IS INITIAL.
      CALL METHOD cl_bds_document_set=>create_with_table
        EXPORTING
          classname       = classname
          classtype       = classtype
          components      = lt_components
          content         = lt_bitmap_bds
        CHANGING
          object_key      = lv_object_key
          signature       = lt_signature
        EXCEPTIONS
          internal_error  = 1
          error_kpro      = 2
          parameter_error = 3
          not_authorized  = 4
          not_allowed     = 5
          nothing_found   = 6
          OTHERS          = 7.
      IF sy-subrc EQ 0.
        READ TABLE lt_signature INTO ls_signature INDEX 1.
        IF sy-subrc EQ 0.
          lv_docid = ls_signature-doc_id.
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
    ELSE.
      CALL METHOD cl_bds_document_set=>update_with_table
        EXPORTING
          classname       = classname
          classtype       = classtype
          doc_id          = lv_docid
          doc_ver_no      = '1'
          doc_var_id      = '1'
          object_key      = lv_object_key
*         x_force_update  = ''
        CHANGING
          components      = lt_components
          content         = lt_bitmap_bds
          signature       = lt_signature
*         properties      =
        EXCEPTIONS
          nothing_found   = 1
          internal_error  = 2
          error_kpro      = 3
          parameter_error = 4
          not_authorized  = 5
          not_allowed     = 6
          OTHERS          = 7.
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

    ENDIF.

* Save bitmap header in STXBITPMAPS
    ls_stxbitmaps-tdname     = iv_imagename.
    ls_stxbitmaps-tdobject   = 'GRAPHICS'.
    ls_stxbitmaps-tdid       = 'BMAP'.
    ls_stxbitmaps-tdbtype    = 'BCOL'.
    ls_stxbitmaps-docid      = lv_docid.
    ls_stxbitmaps-widthpix   = lv_widthpix.
    ls_stxbitmaps-heightpix  = lv_heightpix.
    ls_stxbitmaps-widthtw    = lv_widthtw.
    ls_stxbitmaps-heighttw   = lv_heighttw.
    ls_stxbitmaps-resolution = lv_resolution.
    ls_stxbitmaps-resident   = space.
    ls_stxbitmaps-autoheight = 'X'.
    ls_stxbitmaps-bmcomp     = space.
    MODIFY stxbitmaps FROM ls_stxbitmaps.
    IF sy-subrc <> 0.
      MESSAGE e285(td) WITH iv_imagename 'stxbitmaps' RAISING error.
    ENDIF.


  ENDMETHOD.


  METHOD get_image.

    DATA: lv_object_key TYPE sbdst_object_key.

    lv_object_key = imagename.

    CLEAR:  exist.
    REFRESH: content,
             signature,
             components.

    CALL METHOD cl_bds_document_set=>get_with_table
      EXPORTING
        classname       = classname
        classtype       = classtype
        object_key      = lv_object_key
      CHANGING
        content         = content
        signature       = signature
        components      = components
      EXCEPTIONS
        error_kpro      = 1
        internal_error  = 2
        nothing_found   = 3
        no_content      = 4
        parameter_error = 5
        not_authorized  = 6
        not_allowed     = 7
        OTHERS          = 8.
    IF signature[] IS NOT INITIAL.
      exist = 'X'.
    ENDIF.
  ENDMETHOD.


  METHOD get_image_from_appl.

    TRY.
        OPEN DATASET filename IN BINARY MODE FOR INPUT.
        IF sy-subrc EQ 0.
          READ DATASET filename INTO data.
          CLOSE DATASET filename.
        ELSE.
          MESSAGE e398(00) WITH TEXT-004 RAISING error.
        ENDIF.
      CATCH cx_root INTO DATA(lox_root).
        MESSAGE e398(00) WITH lox_root->if_message~get_text( ) RAISING error.
    ENDTRY.

  ENDMETHOD.


  METHOD GET_IMAGE_FROM_DOCSERVER.

    DATA: lob_http_client TYPE REF TO if_http_client,
          lob_attachment  TYPE REF TO zcl_attachment,
          lt_filename     TYPE zbc_tt_filetable,
          ls_filename     TYPE zbc_s_filetable,
          lt_file_return  TYPE zbc_tt_fdownload_return,
          ls_file_return  TYPE zbc_s_fdownload_return.

    DATA: dummy TYPE string,
          subrc TYPE sysubrc.


    IF url IS INITIAL.
      MESSAGE e000(sr) WITH 'Please provide URL' RAISING error.
    ENDIF.

    IF lob_attachment IS INITIAL.
      CREATE OBJECT lob_attachment.
    ENDIF.

    REFRESH: lt_filename.
    CLEAR: ls_filename .
    ls_filename-dockey = url.
    APPEND ls_filename TO lt_filename.

    CALL METHOD lob_attachment->file_download
      EXPORTING
        it_filename    = lt_filename
        iv_direct_down = ''
      RECEIVING
        rt_return      = lt_file_return.

    CLEAR: ls_file_return.
    READ TABLE lt_file_return INTO ls_file_return INDEX 1.
    IF ls_file_return-type = 'E'.
      MESSAGE e000 WITH ls_file_return-message RAISING error.
    ELSE.
      data = ls_file_return-data.
    ENDIF.

*
*    " Create an instance of the HTTP client:
*    CALL METHOD cl_http_client=>create_by_url
*      EXPORTING
*        url                = url
*      IMPORTING
*        client             = lob_http_client
*      EXCEPTIONS
*        argument_not_found = 1
*        plugin_not_active  = 2
*        internal_error     = 3
*        OTHERS             = 4.
*    IF sy-subrc <> 0.
*      MESSAGE e000(sr) WITH 'Create of client object failed' RAISING error.
*    ENDIF.
*
*    "authenticate
*    CALL METHOD lob_http_client->authenticate
*      EXPORTING
*        username = user
*        password = pwd
*        language = sy-langu.
*
*
*
**    lob_http_client->request->set_method( 'GET' ).
*
*    CALL METHOD lob_http_client->send
*      EXPORTING
*        timeout                    = if_http_client=>co_timeout_default
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3
*        http_invalid_timeout       = 4
*        OTHERS                     = 5.
*    IF sy-subrc <> 0.
*
*      CALL METHOD lob_http_client->get_last_error
*        IMPORTING
*          code    = subrc
*          message = dummy.
*      MESSAGE e398(00) WITH TEXT-002 RAISING error.
*    ENDIF.
*
*
*    CALL METHOD lob_http_client->receive
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3
*        OTHERS                     = 4.
*    IF sy-subrc <> 0.
*
*      CALL METHOD lob_http_client->get_last_error
*        IMPORTING
*          code    = subrc
*          message = dummy.
*      MESSAGE e398(00) WITH TEXT-002 RAISING error.
*
*    ELSE.
*      data = lob_http_client->response->get_data( ).
*    ENDIF.
*
*    CALL METHOD lob_http_client->close.

  ENDMETHOD.


  METHOD get_image_from_local.
    DATA: lt_mime_data  TYPE w3mimetabtype,
          lv_filelength TYPE i,
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
        MESSAGE e398(00) WITH lox_root->if_message~get_text( ) RAISING error.
    ENDTRY.
  ENDMETHOD.


method TABLE_TO_XSTRING .
  FIELD-SYMBOLS: <line> TYPE any,
                 <XFS>  type any.

  CLEAR data_string.
  LOOP AT data_table ASSIGNING <line>.
    assign <line> to <xfs> casting type x.
    CONCATENATE data_string <xfs> INTO data_string IN BYTE MODE.
  ENDLOOP.

ENDMETHOD.


  METHOD upload_image.
    DATA: lv_filename         TYPE string,
          lt_filename         TYPE TABLE OF string,
          lv_mimetype         TYPE string,
          lv_xstring          TYPE xstring,
          lt_blob             TYPE w3mimetabtype,
          lv_image_size       TYPE w3contlen,
          lt_blob_bmp         TYPE w3mimetabtype,
          lv_bmp_size         TYPE w3contlen,
          lob_image_converter TYPE REF TO cl_igs_image_converter,
          lv_perfix           TYPE char10,
          lv_type             TYPE char10.


    lv_type     = iv_type.

    SPLIT iv_filename AT ',' INTO TABLE lt_filename.
    IF lt_filename[] IS NOT INITIAL.
      READ TABLE lt_filename INTO lv_filename INDEX 1.
    ELSE.
      lv_filename = iv_filename.
    ENDIF.

    TRY.
*        lv_perfix = lv_filename+0(4).
*        TRANSLATE lv_perfix TO UPPER CASE.

        CASE lv_type .
          WHEN c_type_docserver.
            "下载网页图片,或者文档服务器的图片

            CALL METHOD get_image_from_docserver
              EXPORTING
                url    = lv_filename
              IMPORTING
                data   = lv_xstring
              EXCEPTIONS
                error  = 1
                OTHERS = 2.
            IF sy-subrc <> 0.
            ENDIF.

          WHEN c_type_appl.
            "打开文件服务器的文件
            CALL METHOD get_image_from_appl
              EXPORTING
                filename = lv_filename
              IMPORTING
                data     = lv_xstring
              EXCEPTIONS
                error    = 1
                OTHERS   = 2.
            IF sy-subrc <> 0.
              CALL METHOD get_image_from_local
                EXPORTING
                  filename = iv_filename
                IMPORTING
                  data     = lv_xstring
                EXCEPTIONS
                  error    = 1
                  OTHERS   = 2.
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

            ENDIF.

          WHEN c_type_local.

            CALL METHOD get_image_from_local
              EXPORTING
                filename = iv_filename
              IMPORTING
                data     = lv_xstring
              EXCEPTIONS
                error    = 1
                OTHERS   = 2.
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
          WHEN OTHERS.
        ENDCASE.


        IF lv_xstring IS INITIAL.
          MESSAGE e398(00) WITH TEXT-003 RAISING error.
        ENDIF.

        lv_image_size = xstrlen( lv_xstring ).

        TRY.
            "获取图片的类型
            DATA: lv_xres     TYPE i,
                  lv_yres     TYPE i,
                  lv_xdpi     TYPE i,
                  lv_ydpi     TYPE i,
                  lv_bitdepth TYPE i.

            CALL METHOD cl_fxs_image_info=>determine_info
              EXPORTING
                iv_data     = lv_xstring
              IMPORTING
                ev_mimetype = lv_mimetype
                ev_xres     = lv_xres "宽
                ev_yres     = lv_yres "高
                ev_xdpi     = lv_xdpi
                ev_ydpi     = lv_ydpi
                ev_bitdepth = lv_bitdepth.

          CATCH cx_fxs_image_unsupported INTO DATA(lob_fxs_image_unsupported).
            MESSAGE e398(00) WITH lob_fxs_image_unsupported->if_message~get_text( )
                      RAISING error .
        ENDTRY.

        "转换图片格式到BMP格式
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer     = lv_xstring
          TABLES
            binary_tab = lt_blob.

        IF lob_image_converter IS INITIAL.
          CREATE OBJECT lob_image_converter.
        ENDIF.

        lob_image_converter->input  = lv_mimetype.
        lob_image_converter->output = format_bmp.

        "1寸295*413(1寸)
        IF lv_xres >  295 OR lv_yres >  413.
          lob_image_converter->width  = 295.
          lob_image_converter->height = 413.
        ELSE.
          lob_image_converter->width  = lv_xres.
          lob_image_converter->height = lv_yres.
        ENDIF.

        CALL METHOD lob_image_converter->set_image
          EXPORTING
            blob      = lt_blob
            blob_size = lv_image_size.
        "转换
        CALL METHOD lob_image_converter->execute
          EXCEPTIONS
            communication_error = 1
            internal_error      = 2
            external_error      = 3
            OTHERS              = 4.
        IF sy-subrc <> 0.
          MESSAGE e398(00) WITH TEXT-001 RAISING error.
        ELSE.
          "获取转换好的图片
          CALL METHOD lob_image_converter->get_image
            IMPORTING
              blob      = lt_blob_bmp
              blob_size = lv_bmp_size.
        ENDIF.

        "Bitmap convert to BDS
        CALL METHOD convert_bitmap_to_bds
          EXPORTING
            it_bitmap_file      = lt_blob_bmp
            iv_bitmap_bytecount = lv_bmp_size
            iv_imagename        = iv_imagename
          EXCEPTIONS
            error               = 1
            OTHERS              = 2.
        IF sy-subrc NE 0.

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
        MESSAGE e398(00) WITH lox_root->if_message~get_text( )
                  RAISING error.
    ENDTRY.



  ENDMETHOD.
ENDCLASS.
