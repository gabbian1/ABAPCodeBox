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

* Subroutine used in YABAP_CODEBOX. DON'T DELETE IT OR SAMARA WILL CALL YOU IN SEVEN DAYS.
PROGRAM yemptybox.

"@Global

"@Parameters

"@Definition

CLASS abap_emptybox DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      run RAISING cx_static_check cx_dynamic_check.
ENDCLASS.

"@Implementation
CLASS abap_emptybox IMPLEMENTATION.
  METHOD run.
"@Statements
  ENDMETHOD.
ENDCLASS.
