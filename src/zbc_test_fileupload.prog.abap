*&---------------------------------------------------------------------*
*& Report ZBC_TEST_FILEUPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbc_test_fileupload.

DATA: lob_attchment TYPE REF TO zcl_attachment,
      lt_return     TYPE zbc_tt_fupload_return,
      ls_return     TYPE zbc_s_fupload_return,
      lt_filetable  TYPE filetable.

IF lob_attchment IS INITIAL.
  CREATE OBJECT lob_attchment.
ENDIF.

lob_attchment->get_token( ).

lob_attchment->file_open_dialog( IMPORTING filetable = lt_filetable ).

lt_return = lob_attchment->file_upload( EXPORTING filetable = lt_filetable ).

LOOP AT lt_return INTO ls_return.
  WRITE:/ ls_return-filename, ls_return-code, ls_return-success, ls_return-data,ls_return-msg.
ENDLOOP.
