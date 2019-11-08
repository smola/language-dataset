IMPORT ML_Core.Types AS Core_Types;
/**
  * Type definitions for LogisticRegression bundle
  */
EXPORT Types := MODULE
  EXPORT AnyField     := Core_Types.AnyField;
  EXPORT NumericField := Core_Types.NumericField;
  EXPORT DiscreteField:= Core_Types.DiscreteField;
  EXPORT Layout_Model := Core_Types.Layout_Model;
  EXPORT t_work_item  := Core_Types.t_work_item;
  EXPORT t_RecordID   := Core_Types.t_RecordID;
  EXPORT t_FieldNumber:= Core_Types.t_FieldNumber;
  EXPORT t_FieldReal  := Core_Types.t_FieldReal;
  EXPORT t_Discrete   := Core_Types.t_discrete;
  EXPORT t_Universe := UNSIGNED1;
  /**
    * Describe information about each field in a training set.
    *
    * @field number the column (feature) number.
    * @field cardinality the number of unique values in the field.
    * @field min_value the minimum value for the field.
    * @field max_value the maximum value for the field.
    */
  EXPORT Field_Desc := RECORD
    t_FieldNumber number;   // the column
    UNSIGNED4 cardinality;  // 0 for too many values
    REAL8 min_value;
    REAL8 max_value;
  END;

  /**
    * Describes information about a training dataset composed of independent
    * and dependent columns.
    *
    * @field wi the work-item number.
    * @field dependent_fields the number of fields in the dependent data.
    * @field dependent_records the number of records in the dependent data.
    * @field independent_fields the number of fields in the independent data.
    * @field independent_records the number of records in the independent data.
    * @field dependent_stats dataset of Field_Desc records describing each of the fields
    *                        of the dependent data.
    * @field independent_stats dataset of Field_Desc records describing each of the fields
    *                        of the independent data.
    * @see Field_Desc
    */
  EXPORT Data_Info := RECORD
    t_work_item wi;
    UNSIGNED4 dependent_fields;     // high number
    UNSIGNED4 dependent_records;    // high ID
    UNSIGNED4 independent_fields;   // high number
    UNSIGNED4 independent_records;  // high ID
    UNSIGNED4 dependent_count;
    UNSIGNED4 independent_count;
    DATASET(Field_Desc) dependent_stats;
    DATASET(Field_Desc) independent_stats;
  END;
  /**
    * Record structure to add a 'Universe Number' to a NumericField
    * allowing multiple independent NumericField matrixes within a
    * work-item.
    *
    * @field u the 'universe' number identifying a distinct matrix
    *          within a NumericField dataset and work-item.
    */
  EXPORT NumericField_U := RECORD(NumericField)
    t_universe u;
  END;
  /**
    * Record structure to add a 'Universe Number' to a DiscreteField
    * allowing multiple independent DiscreteField matrixes within a
    * work-item.
    *
    * @field u the 'universe' number identifying a distinct matrix
    *          within a DiscreteField dataset and work-item.
    */
  EXPORT DiscreteField_U := RECORD(DiscreteField)
    t_universe u;
  END;
  /**
    * Layout for a column map record that is used
    * to remap column numbers.
    *
    * @field wi the work-item number.
    * @field orig_number the original field number.
    * @field remap_number the mapped-to field number.
    *
    */
  EXPORT Layout_Column_Map := RECORD
    t_work_item wi;
    t_FieldNumber orig_number;
    t_FieldNumber remap_number;
  END;
  /**
    * Statistics about the effectiveness of each classifier
    * in a model.
    *
    * @field column the classifier field number.
    * @field max_delta the max_delta value for the classifier.
    * @field iterations the number of iterations used to train the classifier.
    * @field correct the number of classes predicted correctly in the training data.
    * @field incorrect the number of classes predicted incorrectly in the training data.
    */
  EXPORT Classifier_Stats := RECORD
    t_FieldNumber   column;
    t_FieldReal     max_delta;
    UNSIGNED4       iterations;
    UNSIGNED4       correct;
    UNSIGNED4       incorrect;
  END;
  /**
    * Statistical information about a model.
    * <p>One record is generated per work-item.
    *
    * @field wi the work-item
    * @field max_iterations the maximum iterations use to train the model.
    * @field epsilon the 'epsilon' value used within the model.
    * @field dep_vars the number of dependent variables (i.e. classifiers).
    * @field ind_vars the number of independent variables (i.e. features).
    * @field obs the number of observations (i.e. records) in the training data.
    * @field builder the identifier for the builder used to train the model.
    * @field stats child dataset of Classifier_Stats, one for each classifier in the
    *              work-item.
    * @see Classifier_Stats
    */
  EXPORT Model_Report := RECORD
    t_work_item wi;
    UNSIGNED4   max_iterations;
    REAL8       epsilon;
    UNSIGNED4   dep_vars;
    UNSIGNED4   ind_vars;
    UNSIGNED8   obs;
    UNSIGNED2   builder;
    DATASET(Classifier_Stats) stats;
  END;
  /**
    * Accuracy stats for binomial classifications.
    * <p>One record per work-item and classifier.
    *
    * @field wi the work-item number.
    * @field classifier the classifier field number (i.e. dependent field number).
    * @field true_positive the count of true positive results (i.e. predicted = TRUE, actual = TRUE).
    * @field true_negative the count of true negative results (i.e. predicted = FALSE, actual = FALSE).
    * @field false_positive the count of false_positive results (i.e. predicted = TRUE, actual = FALSE).
    * @field false_negative the count of false_negative results (i.e. predicted = FALSE, actual = TRUE).
    * @field cond_pos the count of results where actual = TRUE.
    * @field pred_pos the count of results where predicted = TRUE.
    * @field cond_neg the count of results where actual = FALSE.
    * @field pred_neg the count of results where predicted = FALSE.
    * @field prevalence cond_pos / total.
    * @field accuracy (true_positive + true_negative) / total.
    * @field true_pos_rate true_positive / cond_pos.
    * @field false_pos_rate false_positive / cond_neg.
    * @field true_neg_rate true_negative / cond_neg.
    * @field pos_pred_val true_positive / pred_pos.
    * @field false_disc_rate false_positive / pred_pos.
    * @field false_omit_rate false_negative / pred_neg.
    * @field neg_pred_val true_negative / pred_neg.
    */
  EXPORT Binomial_Confusion_Summary := RECORD
    t_work_item wi;
    t_FieldNumber classifier;
    UNSIGNED8 true_positive;
    UNSIGNED8 true_negative;
    UNSIGNED8 false_positive;
    UNSIGNED8 false_negative;
    UNSIGNED8 cond_pos;       //condition positive
    UNSIGNED8 pred_pos;       //predicted condition positive
    UNSIGNED8 cond_neg;       //condition negative
    UNSIGNED8 pred_neg;       //predicted condition negative
    REAL8 prevalence;         //condition positives/total
    REAL8 accuracy;           //true positives+true negatives/total
    REAL8 true_pos_rate;      //true positives/condition positives
    REAL8 false_neg_rate;     //false negatives/condition positives
    REAL8 false_pos_rate;     //false positives/condition negatives
    REAL8 true_neg_rate;      //true negatives/condition negatives
    REAL8 pos_pred_val;       //true positives/predicted condition positive
    REAL8 false_disc_rate;    //false positives/predicted condition positives
    REAL8 false_omit_rate;    //false negatives /predicted condition negatives
    REAL8 neg_pred_val;       //true negatives/predicted condition negatives
  END;
  /**
    * Model Coefficients.
    *
    * @field wi the work-item number.
    * @field ind_col the independent column number (i.e feature number).
    * @field dep_nom the dependent column number (i.e. classifier number).
    * @field w the learned weight (i.e. coefficient).
    * @field SE the Standard Error of the coefficient.
    */
  EXPORT Model_Coef := RECORD
    t_work_item wi;
    t_FieldNumber ind_col;
    t_FieldNumber dep_nom;
    t_FieldReal w;  // weight
    t_FieldReal SE; // standard error
  END;
  /**
    * Model Coefficients with confidence intervals.
    *
    * @field upper the upper range of the confidence interval
    * @field lower the lower range of the confidence interval
    */
  EXPORT Confidence_Model_Coef := RECORD(Model_Coef)
    REAL8 upper;
    REAL8 lower;
  END;
  /**
    * Model coefficients with z and p-value.
    *
    * @field z the z value.
    * @field p_value the p_value of the coefficient.
    */
  EXPORT pval_Model_Coef := RECORD(Model_Coef)
    REAL8 z;
    REAL8 p_value;
  END;
  /**
    * Model coefficients with confidence intervals and p-value
    *
    * @field z the z value.
    * @field p_value the p_value of the coefficient.
    * @field upper the upper range of the confidence interval
    * @field lower the lower range of the confidence interval
    */
  EXPORT Full_Model_Coef := RECORD(Model_Coef)
    REAL8 z;
    REAL8 p_value;
    REAL8 upper;
    REAL8 lower;
  END;
  /**
    * Model coefficients, confidence intervals, and p-value, plus
    * independent field names, for each coefficient.
    *
    * @field isIntercept Boolean field is TRUE if this is the intercept
    *                    coefficient, otherwise FALSE.
    * @field field_name the name of the independent field for this coefficient.
    * @field w the coefficient value (weight)
    * @field SE the Standard Error of the coefficient
    * @field z the z value.
    * @field p_value the p-value.
    * @field upper the upper bound of the confidence interval.
    * @field lower the lower bound of the confidence interval.
    * @field ind_col the field number of the independent field for this coefficient.
    */
  EXPORT External_Coef := RECORD
    BOOLEAN isIntercept;
    STRING field_name;
    t_FieldReal w;
    t_FieldReal SE;
    REAL8 z;
    REAL8 p_value;
    REAL8 upper;
    REAL8 lower;
    t_FieldNumber ind_col;
  END;
  /**
    * Expanded version of a model with statistics and field names.
    * <p>Field names include independent data field names, dependent
    * data field names and work-item names.
    *
    * @field work_item the work-item's name.
    * @field response_field the name of the classifier field (i.e. dependent field name).
    * @field wi the work-item number.
    * @field dep_nom the field number of the classifier (i.e. dependent field number).
    * @field coef child dataset of External_Coef format.  One record per model coefficient.
    * @see External_Coef
    */
  EXPORT External_Model := RECORD
    STRING work_item;
    STRING response_field;
    t_work_item wi;
    t_FieldNumber dep_nom;
    DATASET(External_Coef) coef;
  END;
  /**
    * Record for raw prediction without confidence information.
    *
    * @field raw the raw prediction value.
    */
  EXPORT Raw_Prediction := RECORD(AnyField)
    REAL8 raw;
  END;
  /**
    * Record to contain deviance information about each observation.
    *
    * @field wi the work-item number.
    * @field id the record id (i.e. observation number).
    * @field classifier the dependent field number.
    * @field actual the actual (i.e. ground truth value).
    * @field predicted the value predicted by the model.
    * @field mod_ll log likelihood of the model
    * @field mod_dev_component the deviance explained by the model 
    * @field mod_dev_residual the deviance not explained by the model (i.e. the residual)
    * @field nil ll log likelihood of the nil model (i.e. model with only a constant term).
    * @field nil_dev_component the deviance explained by the null model
    * @field nil_dev_residual the deviance not explained by the null model (i.e. the residual)
    */
  EXPORT Observation_Deviance := RECORD
    t_work_item wi;
    t_RecordID id;
    t_FieldNumber classifier;
    t_Discrete actual;
    t_Discrete predicted;
    REAL8 mod_ll;
    REAL8 mod_dev_component;
    REAL8 mod_dev_residual;
    REAL8 nil_ll;
    REAL8 nil_dev_component;
    REAL8 nil_dev_residual;
  END;
  /**
    * Record to hold deviance summary information about a model.
    *
    * @field wi the work-item number
    * @field classifier the classifier number (i.e. field number of the dependent
    *                   variable).
    * @field df degrees-of-freedom of the chi squared distribution.
    * @field deviance the total deviance for this classifier.
    * @field AIC the Akaike Information Criteria value.
    */
  EXPORT Deviance_Record := RECORD
    t_work_item wi;
    t_FieldNumber classifier;
    UNSIGNED8 df; // degrees of freedom
    REAL8 deviance;
    REAL8 AIC;
  END;
  /**
    * Record to hold Analysis of Deviance (AOD) information
    * for a model.
    *
    * @field wi the work-item number
    * @field classifier the classifier number (i.e. field number of the dependent
    *                   variable).
    * @field df degrees of freedom of the chi squared distribution.
    * @field residual_dev the deviance not explained by the model.
    * @field deviance the total deviance.
    * @field p value the probability that the null hypothesis is correct.
    */
  EXPORT AOD_Record := RECORD
    t_work_item wi;
    t_FieldNumber classifier;
    UNSIGNED8 residual_df;
    UNSIGNED8 df;
    REAL8 residual_dev;
    REAL8 deviance;
    REAL8 p_value;
  END;
  /**
    * Layout used to hold the mapping between a field's number and its
    * name.
    *
    * @field orig_name typically the field number as a text string (e.g. '2').
    * @field assigned_name the textual name of the field (e.g. 'age').
    */
  EXPORT FieldName_Mapping := RECORD
    STRING orig_name;
    STRING assigned_name;
  END;
  /**
    * Layout used to hold the mapping between a work-item number and a textual
    * name for that work-item.
    *
    * @field wi the work-item number.
    * @field orig_wi the work-item name.
    */
  EXPORT WorkItem_Mapping := RECORD
    t_work_item wi;
    STRING orig_wi;
  END;
  /**
    * Layout to store the lines of a generated LUCI model file.
    *
    * @field line the text for a single line for the LUCI file.
    */
  EXPORT LUCI_Rec := RECORD
    STRING line;
  END;
  /**
    * Format for information to guide the generation of a LUCI file.
    *
    * @field model_id a short textual name for the model as used in the LUCI L1MD format.
    * @field model_name an expanded name for the model as used in the LUCI L1MD format.
    * @field response_field name of the dependent field (aka classifier name).
    * @field wi_list can be set to ['ALL'], or can be a list of work-item names.
    * @field score_card_name the score card name pattern (see LUCI_Model.ecl for details).
    */
  EXPORT LUCI_Model_Rqst := RECORD
    STRING model_id;        // model id
    STRING model_name;      // name on L1MD
    STRING response_field;  // name of the dependent field used in training
    SET OF STRING wi_list;  // can be ALL or one or more work item names
    STRING score_card_name; // score card name pattern
  END;
END;
