*&---------------------------------------------------------------------*
*& Report ZBC_TEST_FILEDOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbc_test_filedownload.

DATA: lob_attchment TYPE REF TO zcl_attachment,
      lt_return     TYPE zbc_tt_fdownload_return,
      ls_return     TYPE zbc_s_fdownload_return,
      lt_filetable  TYPE filetable,
      ls_filetable  LIKE LINE OF lt_filetable.

IF lob_attchment IS INITIAL.
  CREATE OBJECT lob_attchment.
ENDIF.

CLEAR: ls_filetable.
ls_filetable-filename = 'M00/00/3C/ClkDgl7gvCGAOdLhAMcWsrb_IZA412.pdf'.
APPEND ls_filetable TO lt_filetable.

lt_return = lob_attchment->file_download( EXPORTING it_filename    = lt_filetable
                                                    iv_direct_down = 'X' ).

LOOP AT lt_return INTO ls_return.
  WRITE:/ ls_return-filename, ls_return-code, ls_return-type, ls_return-message.
ENDLOOP.
