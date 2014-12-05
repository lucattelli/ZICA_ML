*&---------------------------------------------------------------------*
*& Z Instant Comprehensive ABAP - Linear Regression Program
*& Copyright (C) 2014 Bruno Lucattelli - lucattelli.com
*& This work is licensed under CC ShareAlike 4.0 International
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ABOUT LINEAR REGRESSION
*----------------------------------------------------------------------*
* This algorithm attempts to predict the value of Y based on the
* features of X and the parameters of THETA.
* Example: Given certain features of a pen, tries to predict it's price
*          To be able to predict, the algorithm needs to be trained.
*          To do so, we provide a matrix (X) of pens. Each row of the
*          matrix represents the features of a single pen.
*          The corresponding price of the pen is provided on a answer
*          vector (Y). The algorithm tries to learn from these examples,
*          and stores the learned parameters inside a vector THETA.
*          Once the computer has learned, it can predict pen prices
*          when it receives parameters for different pens.
*----------------------------------------------------------------------*

REPORT zica_ml_lireg.

*-- sets the base directory (required by include ZICA_ML)
CONSTANTS : c_basepath TYPE c LENGTH 12 VALUE 'C:\ML\LIREG\'.

INCLUDE zica_ml.

CONSTANTS : c_alpha TYPE type_float VALUE '0.01', "-- LR's learning rate
            c_iterations TYPE i VALUE 1500. "-- LR's max iterations

DATA  : gt_x TYPE type_float_matrix, "-- features matrix X
        gt_y TYPE type_float_vector, "-- answers vector Y
        gt_theta TYPE type_float_vector. "-- parameter theta

DATA  : gt_adx TYPE type_float_matrix, "-- AVG/DEV vector for matrix X
        gv_ay TYPE type_float, "-- AVG for vector Y
        gv_dy TYPE type_float, "-- DEV for vector Y
        gv_atheta TYPE type_float, "-- AVG for vector THETA
        gv_dtheta TYPE type_float, "-- DEV for vector THETA
        gt_sx TYPE type_float_matrix, "-- scaled features matrix for X
        gt_sy TYPE type_float_vector, "-- scaled answers vector for Y
        gt_stheta TYPE type_float_vector. "-- scaled parameters for THETA

DATA  : gv_j TYPE type_float. "-- the value of the cost

DATA  : gt_minimized_theta TYPE type_float_vector, "-- the learned THETA parameters
        gt_cost_history TYPE type_float_vector, "-- the cost history for every iteration
        gv_used_iterations TYPE i, "-- the number of iterations needed to minimize J.
        gv_min_theta TYPE type_float. "-- the value for one of the learned THETA parameters

DATA  : gt_xv TYPE type_float_vector, "-- X Vector for hypothesis based on user input
        gt_user_x TYPE type_float_matrix, "-- X Matrix for user input
        gt_meaned_x TYPE type_float_matrix, "-- Meaned X Matrix
        gt_meaned_vector TYPE type_float_vector, "-- Meaned X Vector
        gv_answer TYPE type_float, "-- the Y value for hypothesis based on user input
        gt_answer TYPE type_float_vector, "-- Y Vector to be "unmeaned"
        gt_unmeaned_answer TYPE type_float_vector. "-- Y unmeaned vector

PARAMETER brand TYPE type_float DEFAULT '1500'.
PARAMETER ink_type TYPE type_float DEFAULT '1'.
PARAMETER pen_type TYPE type_float DEFAULT '3'.
PARAMETER age TYPE type_float DEFAULT '36'.

START-OF-SELECTION.

*-- loads the training set
  PERFORM load_matrix_from_file TABLES gt_x USING 'LIREG_MULTI_X.TXT'.
  PERFORM load_vector_from_file TABLES gt_y USING 'LIREG_MULTI_Y.TXT'.

*-- fills the initial theta parameters as zero (lets the machine learn it)
  APPEND '0' TO gt_theta.
  APPEND '0' TO gt_theta.
  APPEND '0' TO gt_theta.
  APPEND '0' TO gt_theta.
  APPEND '-3' TO gt_theta.

*-- runs mean normalization for all features
*-- this prevents float variables from *OVERFLOW* short dumps
  PERFORM matrix_mean_normalization TABLES gt_x gt_sx gt_adx.
  PERFORM vector_mean_normalization TABLES gt_y gt_sy CHANGING gv_ay gv_dy.
  PERFORM vector_mean_normalization TABLES gt_theta gt_stheta CHANGING gv_atheta gv_dtheta.

*-- gets the cost for the first theta parameters
  PERFORM lireg_cost_function TABLES gt_sx gt_sy gt_stheta CHANGING gv_j.

*-- outputs the received cost
  WRITE : / 'The cost for initial THETA is:', gv_j.

*-- executes the linear regression algorithm
  CLEAR gv_j.

  PERFORM lireg_gradient_descent TABLES gt_sx gt_sy gt_stheta
                                        gt_minimized_theta gt_cost_history
                                  USING c_alpha c_iterations
                               CHANGING gv_j gv_used_iterations.

*-- outputs results from linear regression
  WRITE : / 'The minimized cost output from Linear Regression is:', gv_j.
  WRITE / 'The minimized THETA values are:'.
  LOOP AT gt_minimized_theta INTO gv_min_theta.
    WRITE : / sy-tabix, gv_min_theta.
  ENDLOOP.
  WRITE : / 'The required iterations were:', gv_used_iterations.

*-- prepares the features for hypothesis
  APPEND 1 TO gt_xv.
  APPEND brand TO gt_xv.
  APPEND ink_type TO gt_xv.
  APPEND pen_type TO gt_xv.
  APPEND age TO gt_xv.
  APPEND gt_xv TO gt_user_x.
  PERFORM matrix_mean_normalization TABLES gt_user_x gt_meaned_x gt_adx.

*-- run hypothesis for given pen features
  READ TABLE gt_meaned_x INTO gt_meaned_vector INDEX 1.
  PERFORM hypothesis TABLES gt_minimized_theta gt_meaned_vector CHANGING gv_answer.
  APPEND gv_answer TO gt_answer.
  PERFORM vector_unmean_normalization TABLES gt_answer gt_unmeaned_answer USING gv_ay gv_dy.
  READ TABLE gt_unmeaned_answer INTO gv_answer INDEX 1.
  WRITE : / 'The predicted price for your pen is :', gv_answer.

*&---------------------------------------------------------------------*
*&      Form  lireg_gradient_descent
*&---------------------------------------------------------------------*
*       Gradient Descent for Linear Regression with Multiple Features
*----------------------------------------------------------------------*
FORM lireg_gradient_descent TABLES t_x t_y t_theta
                                   t_minimized_theta t_cost_history
                             USING alpha iterations
                          CHANGING cost used_iterations.

  DATA  : m TYPE type_float,
          t_current_theta TYPE type_float_vector,
          theta TYPE type_float,
          j TYPE type_float,
          y TYPE type_float,
          i TYPE i,
          t_xv TYPE type_float_vector,
          h TYPE type_float,
          x TYPE type_float,
          e TYPE type_float,
          new_theta TYPE type_float,
          t_new_theta TYPE type_float_vector,
          current_cost TYPE type_float.

  DESCRIBE TABLE t_y LINES m.

  t_current_theta[] = t_theta[].

  DO iterations TIMES.
    used_iterations = sy-index.
    CLEAR t_new_theta.
    LOOP AT t_current_theta INTO theta.
      j = sy-tabix.
      e = 0.
      LOOP AT t_y INTO y.
        i = sy-tabix.
        READ TABLE t_x INTO t_xv INDEX i.
        PERFORM hypothesis TABLES t_current_theta t_xv CHANGING h.
        READ TABLE t_xv INTO x INDEX j.
        e = e + ( ( h - y ) * x ).
      ENDLOOP.
      new_theta = theta - alpha * ( 1 / m ) * e.
      APPEND new_theta TO t_new_theta.
    ENDLOOP.
    PERFORM lireg_cost_function TABLES t_x t_y t_new_theta CHANGING current_cost.
    APPEND current_cost TO t_cost_history.
    t_current_theta[] = t_new_theta[].
    IF current_cost EQ 0.
      EXIT.
    ENDIF.
  ENDDO.

  t_minimized_theta[] = t_current_theta[].
  cost = current_cost.

ENDFORM.                    "lireg_gradient_descent

*&---------------------------------------------------------------------*
*&      Form  lireg_cost_function
*&---------------------------------------------------------------------*
*       The Cost Function returns the error for a hypothesis prediction
*----------------------------------------------------------------------*
FORM lireg_cost_function TABLES t_x t_y t_theta CHANGING cost.

  DATA : m TYPE i,
         t_xv TYPE type_float_vector,
         h TYPE type_float,
         i TYPE i,
         e TYPE type_float,
         partial_e TYPE type_float,
         y TYPE type_float,
         theta TYPE type_float,
         h_y TYPE type_float.

  DESCRIBE TABLE t_y LINES m.
  m = m.

  LOOP AT t_x INTO t_xv.
    i = sy-tabix.
    CLEAR h.
    PERFORM hypothesis TABLES t_theta t_xv CHANGING h.
    READ TABLE t_y INTO y INDEX i.
    e = e + ( ( h - y ) ** 2 ).
  ENDLOOP.

  cost = ( 1 / ( 2 * m ) ) * e.

ENDFORM.                    "lireg_cost_function