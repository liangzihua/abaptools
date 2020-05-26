*&---------------------------------------------------------------------*
*& Report ZIMAGE_UPLOAD_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zimage_upload_test.

PARAMETERS: p_file   TYPE string .
PARAMETERS: p_image  TYPE tdobname .

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
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.
  IF sy-subrc EQ 0.
    MESSAGE s398(00) WITH TEXT-001 .
  ELSE.
    MESSAGE s398(00) WITH TEXT-002 DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
