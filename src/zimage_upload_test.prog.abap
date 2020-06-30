*&---------------------------------------------------------------------*
*& Report ZIMAGE_UPLOAD_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zimage_upload_test.

PARAMETERS: p_file   TYPE string .
PARAMETERS: p_image  TYPE tdobname DEFAULT 'HRPA_NO_IMAGE' .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_select_file CHANGING p_file.

START-OF-SELECTION.
  PERFORM frm_upload_image.


*&---------------------------------------------------------------------*
*& Form FRM_UPLOAD_IMAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_upload_image .
  DATA: lob_image TYPE REF TO zcl_image.

  CREATE OBJECT lob_image.

  CALL METHOD lob_image->upload_image
    EXPORTING
      iv_filename  = p_file
      iv_imagename = p_image
      iv_type      = 'LOCAL'
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.
  IF sy-subrc EQ 0.
    MESSAGE s398(00) WITH TEXT-001 .
  ELSE.
    MESSAGE s398(00) WITH TEXT-002 DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SELECT_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM frm_select_file  CHANGING pv_file.

  DATA: lv_title       TYPE string,
        lv_dir         TYPE string,
        lt_filetable   TYPE filetable,
        ls_filetable   TYPE file_table,
        lv_rc          TYPE i,
        lv_user_action TYPE i.

  lv_title   = '选择文件'.
  lv_dir     = 'c:\'.

  CLEAR: pv_file.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_title
      initial_directory       = lv_dir
*     multiselection          =
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
      user_action             = lv_user_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  READ TABLE lt_filetable INTO ls_filetable INDEX 1.
  IF sy-subrc EQ 0.
    pv_file = ls_filetable-filename.
  ENDIF.

ENDFORM.
