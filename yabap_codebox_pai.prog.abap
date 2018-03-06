*&--------------------------------------------------------------------------------*
*& MIT License
*&
*& Copyright (c) 2018 Raphael Pacheco (#Pacheco)
*&
*& Permission is hereby granted, free of charge, to any person obtaining a copy
*& of this software and associated documentation files (the "Software"), to deal
*& in the Software without restriction, including without limitation the rights
*& to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*& copies of the Software, and to permit persons to whom the Software is
*& furnished to do so, subject to the following conditions:
*&
*& The above copyright notice and this permission notice shall be included in all
*& copies or substantial portions of the Software.
*&
*& THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*& IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*& FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*& AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*& LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*& OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*& SOFTWARE.
*&
*& Git Project URL: https://www.github.com/pacheco7/abapcodebox
*&--------------------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'ABORT' OR 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'BT_CHECK'.
    WHEN 'BT_CLEAR'.
      FREE: statements[].
    WHEN 'BT_EXEC'.
      abap_code_box=>run_source( ).

    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).

  ENDCASE.
  CLEAR ok_code.
ENDMODULE.
