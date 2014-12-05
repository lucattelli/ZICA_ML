*&---------------------------------------------------------------------*
*& Z Instant Comprehensive ABAP - Neural Network Prediction Program
*& Copyright (C) 2014 Bruno Lucattelli - lucattelli.com
*& This work is licensed under CC ShareAlike 4.0 International
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ABOUT NEURAL NETWORKS
*----------------------------------------------------------------------*
* This algorithm implements a simple neural network to try to predict
* the digit inside an bitmap image by using computer vision technique.
*----------------------------------------------------------------------*

REPORT  zica_ml_nn.

*-- sets the base directory (required by include ZICA_ML)
CONSTANTS : c_basepath TYPE c LENGTH 9 VALUE 'C:\ML\NN\'.

INCLUDE zica_ml. "-- includes the ZICA - Machine Learning Library

TYPES : BEGIN OF type_rgb,
          r TYPE x LENGTH 1,
          g TYPE x LENGTH 1,
          b TYPE x LENGTH 1,
        END OF type_rgb,
        type_rgb_table TYPE TABLE OF type_rgb WITH DEFAULT KEY.

TYPES : BEGIN OF type_pixel,
          row TYPE i,
          col TYPE i,
          red TYPE x,
          green TYPE x,
          blue TYPE x,
        END OF type_pixel.

*----------------------------------------------------------------------*
*       CLASS cl_html DEFINITION
*----------------------------------------------------------------------*
* HTML display of images
*----------------------------------------------------------------------*
CLASS cl_html DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF type_html,
              line(255),
            END OF type_html,
            type_html_tab TYPE TABLE OF type_html WITH DEFAULT KEY.

    TYPES : BEGIN OF type_result,
              class TYPE n LENGTH 1,
              value TYPE type_float,
            END OF type_result,
            type_result_table TYPE TABLE OF type_result WITH DEFAULT KEY.

    TYPES : BEGIN OF type_rgb,
              r TYPE x LENGTH 1,
              g TYPE x LENGTH 1,
              b TYPE x LENGTH 1,
            END OF type_rgb,
            type_rgbtab TYPE TABLE OF type_rgb WITH DEFAULT KEY.
    METHODS : constructor IMPORTING w TYPE i h TYPE i,
              sets_prediction IMPORTING result TYPE type_result,
              append_data IMPORTING values TYPE type_rgbtab,
              get_html RETURNING value(data) TYPE type_html_tab.
  PRIVATE SECTION.
    TYPES : BEGIN OF type_imgdata,
              row TYPE i,
              col TYPE i,
              color TYPE type_rgb,
            END OF type_imgdata.
    TYPES : BEGIN OF type_rgb_c,
              r TYPE c LENGTH 2,
              g TYPE c LENGTH 2,
              b TYPE c LENGTH 2,
            END OF type_rgb_c.
    CONSTANTS : c_magnify_factor TYPE i VALUE 10.
    DATA : width TYPE i,
           height TYPE i,
           prediction TYPE type_result,
           t_html TYPE type_html_tab.
    METHODS : add_header,
              add_footer,
              open_line,
              add_pixel IMPORTING rgb TYPE type_rgb,
              close_line.
ENDCLASS.                    "cl_html DEFINITION

*****************************************************
*              CLASS cl_myevent_handler             *
*****************************************************
CLASS cl_myevent_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_navigate_complete
               FOR EVENT navigate_complete OF cl_gui_html_viewer
               IMPORTING url.
ENDCLASS.                    "cl_myevent_handler DEFINITION

DATA  : gv_filename TYPE rlgrap-filename.
DATA  : gt_float_vector TYPE type_float_vector.
DATA  : gt_x TYPE type_float_matrix.
DATA  : gt_theta1 TYPE type_float_matrix.
DATA  : gt_theta2 TYPE type_float_matrix.
DATA  : gt_predict TYPE type_float_matrix.
DATA  : gt_predict_vector TYPE type_float_vector.
DATA  : predict_value TYPE type_float.
DATA  : result TYPE cl_html=>type_result.
DATA  : gt_result TYPE cl_html=>type_result_table.
DATA  : gt_rgb TYPE type_rgb_table.
DATA  : gt_rgb_rotated TYPE type_rgb_table.
DATA  : go_html TYPE REF TO cl_html.
DATA: : go_evt_receiver TYPE REF TO cl_myevent_handler.

DATA : html_control TYPE REF TO cl_gui_html_viewer,
       my_container TYPE REF TO cl_gui_custom_container,
       fcode LIKE sy-ucomm,
       myevent_tab TYPE cntl_simple_events,
       myevent TYPE cntl_simple_event,
       edurl(2048),
       alignment TYPE i,
       html TYPE REF TO cl_html.

START-OF-SELECTION.

* chooses image for prediction
  PERFORM choose_file CHANGING gv_filename.
  CHECK NOT gv_filename IS INITIAL.

* Loads binary image
  PERFORM load_image_to_rgb TABLES gt_rgb USING gv_filename.

* rotates image (required for prediction)
  PERFORM rotates_bitmap TABLES gt_rgb gt_rgb_rotated.

* converts the RGB values into something we can use (float values)
  PERFORM convert_rgb_to_float TABLES gt_rgb_rotated gt_float_vector.
  APPEND gt_float_vector TO gt_x.

* load the learned theta parameters
  PERFORM load_matrix_from_file TABLES gt_theta1 USING 'THETA1.TXT'.
  PERFORM load_matrix_from_file TABLES gt_theta2 USING 'THETA2.TXT'.

* runs prediction
  PERFORM nn_predict TABLES gt_theta1 gt_theta2 gt_x gt_predict.

* interprets the result for prediction
  READ TABLE gt_predict INTO gt_predict_vector INDEX 1.
  LOOP AT gt_predict_vector INTO predict_value.
    result-class = sy-tabix.
    result-value = predict_value.
    APPEND result TO gt_result.
  ENDLOOP.
  SORT gt_result BY value DESCENDING.
  READ TABLE gt_result INTO result INDEX 1.

* generates output
  PERFORM output.

*&---------------------------------------------------------------------*
*&      Form  choose_file
*&---------------------------------------------------------------------*
*       Choose a filepath from the filesystem
*----------------------------------------------------------------------*
FORM choose_file CHANGING filename.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask          = '*.bmp'
      static        = 'X'
    CHANGING
      file_name     = filename
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  CHECK NOT filename IS INITIAL.
ENDFORM.                    "choose_file

*&---------------------------------------------------------------------*
*&      Form  import_file
*&---------------------------------------------------------------------*
*       Imports a binary image into
*----------------------------------------------------------------------*
FORM import_file TABLES pixel_array USING filename.


  DATA  : t_rgb TYPE type_rgb_table.



ENDFORM.                    "import_file

*&---------------------------------------------------------------------*
*&      Form  rotates_bitmap
*&---------------------------------------------------------------------*
*       Rotates a bitmap image CW.
*----------------------------------------------------------------------*
FORM rotates_bitmap TABLES import_rgb export_rgb.
  DATA  : t_irgb TYPE type_rgb_table.
  DATA  : t_orgb TYPE type_rgb_table.
  DATA  : pixel TYPE type_pixel.
  DATA  : rgbval TYPE type_rgb.
  DATA  : t_pixel_array TYPE TABLE OF type_pixel WITH DEFAULT KEY.
  FIELD-SYMBOLS : <pixel> TYPE type_pixel.

  t_irgb[] = import_rgb[].

* rotates the image for analysis
  pixel-row = 1.
  LOOP AT t_irgb INTO rgbval.
    IF pixel-col EQ 20.
      pixel-row = pixel-row + 1.
      pixel-col = 0.
    ENDIF.
    pixel-col = pixel-col + 1.
    pixel-red = rgbval-r.
    pixel-green = rgbval-g.
    pixel-blue = rgbval-b.
    APPEND pixel TO t_pixel_array.
  ENDLOOP.

  LOOP AT t_pixel_array ASSIGNING <pixel>.
    CLEAR pixel.
    pixel = <pixel>.
    pixel-row = <pixel>-col.
    pixel-col = 21 - <pixel>-row.
    <pixel> = pixel.
  ENDLOOP.
  SORT t_pixel_array BY row col.

  LOOP AT t_pixel_array INTO pixel.
    CLEAR rgbval.
    rgbval-r = pixel-red.
    rgbval-g = pixel-green.
    rgbval-b = pixel-blue.
    APPEND rgbval TO t_orgb.
  ENDLOOP.

  export_rgb[] = t_orgb[].

ENDFORM.                    "rotates_bitmap

*&---------------------------------------------------------------------*
*&      Form  convert_rgb_to_float
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM convert_rgb_to_float TABLES rgb f.
  DATA  : t_rgb TYPE type_rgb_table,
          rgbval TYPE type_rgb,
          float TYPE type_float.
  t_rgb[] = rgb[].
* converts RGB vals to float
  LOOP AT t_rgb INTO rgbval.
    float = 1 - ( ( ( rgbval-r + rgbval-g + rgbval-b ) / 3 ) / 255 ).
*    unscaled_float = ( float + scale-min ) * ( scale-max - scale-min ).
    APPEND float TO f.
  ENDLOOP.
ENDFORM.                    "convert_rgb_to_float

*&---------------------------------------------------------------------*
*&      Form  load_image_to_rgb
*&---------------------------------------------------------------------*
*       Loads the image file and converts it into a T_RGB data.
*----------------------------------------------------------------------*
FORM load_image_to_rgb TABLES t_rgb USING file.
  DATA  : t_binary TYPE TABLE OF x WITH DEFAULT KEY.
  DATA  : byte TYPE x.
  DATA  : offset TYPE i.
  DATA  : idx TYPE i.
  DATA  : rgb TYPE type_rgb.
  PERFORM load_bin_file TABLES t_binary USING file.
  READ TABLE t_binary INTO byte INDEX 11.
  offset = byte + 1.
  CLEAR byte.
  LOOP AT t_binary INTO byte FROM offset.
    idx = idx + 1.
    CASE idx.
      WHEN 1. rgb-r = byte.
      WHEN 2. rgb-g = byte.
      WHEN 3. rgb-b = byte.
    ENDCASE.
    IF idx = 3.
      APPEND rgb TO t_rgb.
      CLEAR rgb.
      idx = 0.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "load_image_to_rgb

*&---------------------------------------------------------------------*
*&      Form  get_z
*&---------------------------------------------------------------------*
*       Calculates hypothesis for all occurrencies of X'*theta
*----------------------------------------------------------------------*
FORM get_z TABLES ix itheta oz.

  DATA  : t_x TYPE type_float_matrix,
          t_theta TYPE type_float_vector,
          m TYPE i,
          t_xv TYPE type_float_vector,

          t_data_matrix TYPE type_float_matrix,
          t_data_vector TYPE type_float_vector,
          data_item TYPE type_float,
          t_y TYPE type_float_vector,
          z TYPE type_float,
          t_zv TYPE type_float_vector.

  t_x[] = ix[].
  t_theta[] = itheta[].
  DESCRIBE TABLE t_x LINES m.
  DO m TIMES.
    READ TABLE t_x INTO t_xv INDEX sy-index.
    CLEAR z.
    PERFORM hypothesis TABLES t_theta t_xv CHANGING z.
    APPEND z TO t_zv.
  ENDDO.

  oz[] = t_zv[].

ENDFORM.                    "get_z

*&---------------------------------------------------------------------*
*&      Form  sigmoid_function
*&---------------------------------------------------------------------*
*       Implements the Sigmoid Function for Logistic Regression
*----------------------------------------------------------------------*
FORM sigmoid_function TABLES iz og.
  DATA  : t_z TYPE type_float_vector,
          z TYPE type_float,
          t_g TYPE type_float_vector,
          g TYPE type_float.

  t_z[] = iz[].

  LOOP AT t_z INTO z.
    g = 1 / ( 1 + c_e ** ( -1 * z ) ).
    APPEND g TO t_g.
  ENDLOOP.

  og[] = t_g[].

ENDFORM.                    "sigmoid_function

*&---------------------------------------------------------------------*
*&      Form  nn_predict
*&---------------------------------------------------------------------*
*       Executes Neural Network Prediction for user input data
*----------------------------------------------------------------------*
FORM nn_predict TABLES itheta1 itheta2 ix op.
  DATA  : t_theta1 TYPE type_float_matrix,
          t_theta2 TYPE type_float_matrix,
          t_x TYPE type_float_matrix,
          m TYPE i,
          num_labels TYPE i.
  t_theta1[] = itheta1[].
  t_theta2[] = itheta2[].
  t_x[] = ix[].
  DESCRIBE TABLE t_x LINES m.
  DESCRIBE TABLE t_theta2 LINES num_labels.

  DATA  : t_xv TYPE type_float_vector,
          t_av TYPE type_float_vector,
          t_a1 TYPE type_float_matrix.
* generates A1 from ONE + X
  LOOP AT t_x INTO t_xv.
    CLEAR t_av.
    APPEND 1 TO t_av.
    APPEND LINES OF t_xv TO t_av.
    APPEND t_av TO t_a1.
  ENDLOOP.

* generates Z2 from A1 * THETA1
  DATA  : t_theta1_transposed TYPE type_float_matrix.
  DATA  : t_theta1_vector TYPE type_float_vector.
  DATA  : t_z2v TYPE type_float_vector.
  DATA  : t_z2 TYPE type_float_matrix.
  LOOP AT t_theta1 INTO t_theta1_vector.
    CLEAR t_z2v.
    PERFORM get_z TABLES t_a1 t_theta1_vector t_z2v.
    APPEND t_z2v TO t_z2.
  ENDLOOP.

* generates A2 from ONE + SIGMOID(Z2)
  DATA  : z2 TYPE type_float.
  DATA  : t_a2v TYPE type_float_vector.
  DATA  : t_a2p TYPE type_float_matrix.
  DATA  : t_a2 TYPE type_float_matrix.
  LOOP AT t_z2 INTO t_z2v.
    CLEAR t_a2v.
    IF sy-tabix EQ 1.
      LOOP AT t_z2v INTO z2.
        APPEND 1 TO t_a2v.
      ENDLOOP.
      APPEND t_a2v TO t_a2p.
      CLEAR t_a2v.
    ENDIF.
    PERFORM sigmoid_function TABLES t_z2v t_a2v.
    APPEND t_a2v TO t_a2p.
  ENDLOOP.
  PERFORM matrix_transpose TABLES t_a2p t_a2.

* generates Z3 from A2 * THETA2
  DATA  : t_theta2_transposed TYPE type_float_matrix.
  DATA  : t_theta2_vector TYPE type_float_vector.
  DATA  : t_z3v TYPE type_float_vector.
  DATA  : t_z3 TYPE type_float_matrix.
  LOOP AT t_theta2 INTO t_theta2_vector.
    CLEAR t_z3v.
    PERFORM get_z TABLES t_a2 t_theta2_vector t_z3v.
    APPEND t_z3v TO t_z3.
  ENDLOOP.

* generates A3 from ONE + SIGMOID(Z3)
  DATA  : z3 TYPE type_float.
  DATA  : t_a3v TYPE type_float_vector.
  DATA  : t_a3p TYPE type_float_matrix.
  DATA  : t_a3 TYPE type_float_matrix.
  LOOP AT t_z3 INTO t_z3v.
    CLEAR t_a3v.
    PERFORM sigmoid_function TABLES t_z3v t_a3v.
    APPEND t_a3v TO t_a3p.
  ENDLOOP.
  PERFORM matrix_transpose TABLES t_a3p t_a3.

  op[] = t_a3[].

ENDFORM.                    "nn_predict

*&---------------------------------------------------------------------*
*&      Form  output
*&---------------------------------------------------------------------*
*       Outputs in HTML format
*----------------------------------------------------------------------*
FORM output.

  CREATE OBJECT go_html
    EXPORTING
      w = 20
      h = 20.
  go_html->sets_prediction( result ).
  go_html->append_data( gt_rgb ).
  CALL SCREEN 0100.

ENDFORM.                    "output


*----------------------------------------------------------------------*
*       CLASS cl_html IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_html IMPLEMENTATION.
  METHOD constructor.
    me->width = w.
    me->height = h.
  ENDMETHOD.                    "constructor
  METHOD sets_prediction.
    me->prediction = result.
  ENDMETHOD.                    "sets_prediction
  METHOD append_data.
    TYPES : BEGIN OF type_lines,
              idx TYPE sy-index,
              rgbtab TYPE me->type_rgbtab,
            END OF type_lines.
    DATA : parameter_resolution TYPE sy-tfill,
           required_resolution TYPE sy-tfill,
           rgb TYPE me->type_rgb,
           idx TYPE i,
           line TYPE type_lines,
           t_lines TYPE TABLE OF type_lines WITH DEFAULT KEY.
    DESCRIBE TABLE values LINES parameter_resolution.
    parameter_resolution = parameter_resolution.
    required_resolution = me->width * me->height.
    CHECK parameter_resolution EQ required_resolution.
    DESCRIBE TABLE values LINES line-idx.
    line-idx = 1.
    LOOP AT values INTO rgb.
      idx = idx + 1.
      line-idx = line-idx + 1.
      APPEND rgb TO line-rgbtab.
      IF idx EQ me->width.
        APPEND line TO t_lines.
        CLEAR line-rgbtab.
        idx = 0.
      ENDIF.
    ENDLOOP.
    SORT t_lines BY idx DESCENDING.
    me->add_header( ).
    me->open_line( ).
    LOOP AT t_lines INTO line.
      LOOP AT line-rgbtab INTO rgb.
        idx = idx + 1.
        me->add_pixel( rgb ).
        IF idx EQ me->width.
          me->close_line( ).
          me->open_line( ).
          idx = 0.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    me->close_line( ).
    me->add_footer( ).
  ENDMETHOD.                    "append_data
  METHOD get_html.
    data = me->t_html.
  ENDMETHOD.                    "get_html
  METHOD add_header.
    DATA : html TYPE me->type_html,
           w TYPE c LENGTH 20,
           h TYPE c LENGTH 20.
    w = me->width * me->c_magnify_factor.
    CONDENSE w NO-GAPS.
    h = me->height * me->c_magnify_factor.
    CONDENSE h NO-GAPS.
    APPEND '<html>' TO me->t_html.
    APPEND '<body>' TO me->t_html.
    APPEND '<table width="100%" height="100%" border="0" cellspacing="0" cellpadding="0">' TO me->t_html.
    APPEND '<tr><td  align="center" valign="middle">' TO me->t_html.
    CONCATENATE '<table width="'
                w
                'px" height="'
                h
                'px" border="0" cellspacing="0" cellpadding="0">'
           INTO html-line.
    APPEND html TO me->t_html.
  ENDMETHOD.                    "add_header
  METHOD add_footer.
    APPEND '</table>' TO me->t_html.
    APPEND '</td></tr>' TO me->t_html.
    APPEND '<tr><td>' TO me->t_html.
    APPEND '<center><p><font face="Arial" size="6">Predicted class:' TO me->t_html.
    APPEND me->prediction-class TO me->t_html.
    APPEND 'with a certainty of :' TO me->t_html.
    APPEND me->prediction-value TO me->t_html.
    APPEND '</font></p></center>' TO me->t_html.
    APPEND '</table>' TO me->t_html.
    APPEND '</body>' TO me->t_html.
    APPEND '</html>' TO me->t_html.
  ENDMETHOD.                    "add_footer
  METHOD open_line.
    APPEND '<tr>' TO me->t_html.
  ENDMETHOD.                    "open_line
  METHOD add_pixel.
    DATA : color TYPE type_rgb_c,
           w TYPE c LENGTH 20,
           h TYPE c LENGTH 20,
           html TYPE me->type_html.
    color-r = rgb-r.
    color-g = rgb-g.
    color-b = rgb-b.
    w = me->c_magnify_factor.
    CONDENSE w NO-GAPS.
    h = me->c_magnify_factor.
    CONDENSE h NO-GAPS.
    CONCATENATE '<td width="'
                w
                'px" height="'
                h
                'px" bgcolor="'
                color
                '"></td>'
           INTO html-line.
    APPEND html TO me->t_html.
  ENDMETHOD.                    "add_pixel
  METHOD close_line.
    APPEND '</tr>' TO me->t_html.
  ENDMETHOD.                    "close_line
ENDCLASS.                    "cl_html IMPLEMENTATION

****************************************************
*    cl_myevent_handler implementation             *
****************************************************
CLASS cl_myevent_handler IMPLEMENTATION.

*************************************************************
* DON'T USE the NAVIGATE_COMPLETE event in application logic
*************************************************************
  METHOD on_navigate_complete.
    edurl = url.
  ENDMETHOD.                    "on_navigate_complete

ENDCLASS.                    "cl_myevent_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'HTML'.
  SET TITLEBAR 'HTML'.

  IF my_container IS INITIAL.
    CREATE OBJECT my_container
      EXPORTING
        container_name = 'HTML'
      EXCEPTIONS
        others         = 1.
    CASE sy-subrc.
      WHEN 0.
      WHEN OTHERS.
        RAISE cntl_error.
    ENDCASE.
  ENDIF.

  IF html_control IS INITIAL.
    CREATE OBJECT html_control
      EXPORTING
        parent = my_container.
    IF sy-subrc NE 0.
      RAISE cntl_error.
    ENDIF.

* register event

*************************************************************
* DON'T USE the NAVIGATE_COMPLETE event in application logic
*************************************************************
    myevent-eventid = html_control->m_id_navigate_complete.
    myevent-appl_event = 'X'.
    APPEND myevent TO myevent_tab.
    CALL METHOD html_control->set_registered_events
      EXPORTING
        events = myevent_tab.

    CREATE OBJECT go_evt_receiver.

    SET HANDLER go_evt_receiver->on_navigate_complete
                FOR html_control.

    PERFORM load_home_page.
  ENDIF.
ENDMODULE.                             " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE fcode.
    WHEN 'BACK'.
      IF NOT html_control IS INITIAL.
        CALL METHOD html_control->free.
        FREE html_control.
      ENDIF.
      IF NOT my_container IS INITIAL.
        CALL METHOD my_container->free
          EXCEPTIONS
            OTHERS = 1.
        IF sy-subrc <> 0.
*         MESSAGE E002 WITH F_RETURN.
        ENDIF.
        FREE my_container.
      ENDIF.

      LEAVE PROGRAM.

    WHEN 'HHOM'.                       " show the home page
      CALL METHOD html_control->go_home.

      CALL METHOD html_control->get_current_url
        IMPORTING
          url = edurl.

    WHEN 'HBAK'.
      CALL METHOD html_control->go_back.

      CALL METHOD html_control->get_current_url
        IMPORTING
          url = edurl.

    WHEN 'HFWD'.
      CALL METHOD html_control->go_forward.

      CALL METHOD html_control->get_current_url
        IMPORTING
          url = edurl.

    WHEN 'HRFR'.
      CALL METHOD html_control->do_refresh.

      CALL METHOD html_control->get_current_url
        IMPORTING
          url = edurl.

    WHEN 'HNAV'.
      IF NOT edurl IS INITIAL.
        CALL METHOD html_control->show_url
          EXPORTING
            url                  = edurl
          EXCEPTIONS
            cnht_error_parameter = 1
            OTHERS               = 2.
        IF sy-subrc GE 2.
          RAISE cntl_error.
        ENDIF.
      ENDIF.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.
  CLEAR fcode.
ENDMODULE.                             " USER_COMMAND_0100  INPUT

*----------------------------------------------------------------------*
*  MODULE exit_command_0100
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE exit_command_0100.
  IF sy-ucomm = 'EXIT'.
    SET SCREEN 0.
  ENDIF.
ENDMODULE.                    "exit_command_0100

* Homepage form
FORM load_home_page.

  DATA : t_html TYPE cl_html=>type_html_tab,
         doc_url(80).

  t_html = go_html->get_html( ).

  CALL METHOD html_control->load_data
    IMPORTING
      assigned_url         = doc_url
    CHANGING
      data_table           = t_html
    EXCEPTIONS
      dp_invalid_parameter = 1
      dp_error_general     = 2
      cntl_error           = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF sy-subrc EQ 0.
    CALL METHOD html_control->show_url
      EXPORTING
        url = doc_url.
  ENDIF.
ENDFORM.                               " LOAD_HOME_PAGE