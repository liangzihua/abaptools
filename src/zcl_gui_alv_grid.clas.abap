class ZCL_GUI_ALV_GRID definition
  public
  inheriting from CL_GUI_ALV_GRID
  final
  create public .

public section.

  methods GET_OUTTAB
    returning
      value(RT_OUTTAB) type ref to DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GUI_ALV_GRID IMPLEMENTATION.


  METHOD get_outtab.
    rt_outtab = mt_outtab.
  ENDMETHOD.
ENDCLASS.
