IMPORT * FROM ML;
IMPORT * FROM ML.Types;

dsRecord := RECORD
  Types.t_FieldReal a1 ;
  Types.t_FieldReal a2 ;
  Types.t_FieldReal a3 ;
  Types.t_FieldReal a4 ;
  Types.t_FieldReal class;
END;

irisDS := DATASET([
{5.1,3.5,1.4,0.2,3},
{4.9,3.0,1.4,0.2,3},
{4.7,3.2,1.3,0.2,3},
{4.6,3.1,1.5,0.2,3},
{5.0,3.6,1.4,0.2,3},
{5.4,3.9,1.7,0.4,3},
{4.6,3.4,1.4,0.3,3},
{5.0,3.4,1.5,0.2,3},
{4.4,2.9,1.4,0.2,3},
{4.9,3.1,1.5,0.1,3},
{5.4,3.7,1.5,0.2,3},
{4.8,3.4,1.6,0.2,3},
{4.8,3.0,1.4,0.1,3},
{4.3,3.0,1.1,0.1,3},
{5.8,4.0,1.2,0.2,3},
{5.7,4.4,1.5,0.4,3},
{5.4,3.9,1.3,0.4,3},
{5.1,3.5,1.4,0.3,3},
{5.7,3.8,1.7,0.3,3},
{5.1,3.8,1.5,0.3,3},
{5.4,3.4,1.7,0.2,3},
{5.1,3.7,1.5,0.4,3},
{4.6,3.6,1.0,0.2,3},
{5.1,3.3,1.7,0.5,3},
{4.8,3.4,1.9,0.2,3},
{5.0,3.0,1.6,0.2,3},
{5.0,3.4,1.6,0.4,3},
{5.2,3.5,1.5,0.2,3},
{5.2,3.4,1.4,0.2,3},
{4.7,3.2,1.6,0.2,3},
{4.8,3.1,1.6,0.2,3},
{5.4,3.4,1.5,0.4,3},
{5.2,4.1,1.5,0.1,3},
{5.5,4.2,1.4,0.2,3},
{4.9,3.1,1.5,0.1,3},
{5.0,3.2,1.2,0.2,3},
{5.5,3.5,1.3,0.2,3},
{4.9,3.1,1.5,0.1,3},
{4.4,3.0,1.3,0.2,3},
{5.1,3.4,1.5,0.2,3},
{5.0,3.5,1.3,0.3,3},
{4.5,2.3,1.3,0.3,3},
{4.4,3.2,1.3,0.2,3},
{5.0,3.5,1.6,0.6,3},
{5.1,3.8,1.9,0.4,3},
{4.8,3.0,1.4,0.3,3},
{5.1,3.8,1.6,0.2,3},
{4.6,3.2,1.4,0.2,3},
{5.3,3.7,1.5,0.2,3},
{5.0,3.3,1.4,0.2,3},
{7.0,3.2,4.7,1.4,1},
{6.4,3.2,4.5,1.5,1},
{6.9,3.1,4.9,1.5,1},
{5.5,2.3,4.0,1.3,1},
{6.5,2.8,4.6,1.5,1},
{5.7,2.8,4.5,1.3,1},
{6.3,3.3,4.7,1.6,1},
{4.9,2.4,3.3,1.0,1},
{6.6,2.9,4.6,1.3,1},
{5.2,2.7,3.9,1.4,1},
{5.0,2.0,3.5,1.0,1},
{5.9,3.0,4.2,1.5,1},
{6.0,2.2,4.0,1.0,1},
{6.1,2.9,4.7,1.4,1},
{5.6,2.9,3.6,1.3,1},
{6.7,3.1,4.4,1.4,1},
{5.6,3.0,4.5,1.5,1},
{5.8,2.7,4.1,1.0,1},
{6.2,2.2,4.5,1.5,1},
{5.6,2.5,3.9,1.1,1},
{5.9,3.2,4.8,1.8,1},
{6.1,2.8,4.0,1.3,1},
{6.3,2.5,4.9,1.5,1},
{6.1,2.8,4.7,1.2,1},
{6.4,2.9,4.3,1.3,1},
{6.6,3.0,4.4,1.4,1},
{6.8,2.8,4.8,1.4,1},
{6.7,3.0,5.0,1.7,1},
{6.0,2.9,4.5,1.5,1},
{5.7,2.6,3.5,1.0,1},
{5.5,2.4,3.8,1.1,1},
{5.5,2.4,3.7,1.0,1},
{5.8,2.7,3.9,1.2,1},
{6.0,2.7,5.1,1.6,1},
{5.4,3.0,4.5,1.5,1},
{6.0,3.4,4.5,1.6,1},
{6.7,3.1,4.7,1.5,1},
{6.3,2.3,4.4,1.3,1},
{5.6,3.0,4.1,1.3,1},
{5.5,2.5,4.0,1.3,1},
{5.5,2.6,4.4,1.2,1},
{6.1,3.0,4.6,1.4,1},
{5.8,2.6,4.0,1.2,1},
{5.0,2.3,3.3,1.0,1},
{5.6,2.7,4.2,1.3,1},
{5.7,3.0,4.2,1.2,1},
{5.7,2.9,4.2,1.3,1},
{6.2,2.9,4.3,1.3,1},
{5.1,2.5,3.0,1.1,1},
{5.7,2.8,4.1,1.3,1},
{6.3,3.3,6.0,2.5,2},
{5.8,2.7,5.1,1.9,2},
{7.1,3.0,5.9,2.1,2},
{6.3,2.9,5.6,1.8,2},
{6.5,3.0,5.8,2.2,2},
{7.6,3.0,6.6,2.1,2},
{4.9,2.5,4.5,1.7,2},
{7.3,2.9,6.3,1.8,2},
{6.7,2.5,5.8,1.8,2},
{7.2,3.6,6.1,2.5,2},
{6.5,3.2,5.1,2.0,2},
{6.4,2.7,5.3,1.9,2},
{6.8,3.0,5.5,2.1,2},
{5.7,2.5,5.0,2.0,2},
{5.8,2.8,5.1,2.4,2},
{6.4,3.2,5.3,2.3,2},
{6.5,3.0,5.5,1.8,2},
{7.7,3.8,6.7,2.2,2},
{7.7,2.6,6.9,2.3,2},
{6.0,2.2,5.0,1.5,2},
{6.9,3.2,5.7,2.3,2},
{5.6,2.8,4.9,2.0,2},
{7.7,2.8,6.7,2.0,2},
{6.3,2.7,4.9,1.8,2},
{6.7,3.3,5.7,2.1,2},
{7.2,3.2,6.0,1.8,2},
{6.2,2.8,4.8,1.8,2},
{6.1,3.0,4.9,1.8,2},
{6.4,2.8,5.6,2.1,2},
{7.2,3.0,5.8,1.6,2},
{7.4,2.8,6.1,1.9,2},
{7.9,3.8,6.4,2.0,2},
{6.4,2.8,5.6,2.2,2},
{6.3,2.8,5.1,1.5,2},
{6.1,2.6,5.6,1.4,2},
{7.7,3.0,6.1,2.3,2},
{6.3,3.4,5.6,2.4,2},
{6.4,3.1,5.5,1.8,2},
{6.0,3.0,4.8,1.8,2},
{6.9,3.1,5.4,2.1,2},
{6.7,3.1,5.6,2.4,2},
{6.9,3.1,5.1,2.3,2},
{5.8,2.7,5.1,1.9,2},
{6.8,3.2,5.9,2.3,2},
{6.7,3.3,5.7,2.5,2},
{6.7,3.0,5.2,2.3,2},
{6.3,2.5,5.0,1.9,2},
{6.5,3.0,5.2,2.0,2},
{6.2,3.4,5.4,2.3,2},
{5.9,3.0,5.1,1.8,2}], dsRecord);

OUTPUT(irisDS, NAMED('irisDS'), ALL);
AppendID(irisDS, id, iris_data);          // Add id to iris_data (ML.ToField requires id)
ML.ToField(iris_Data, full_ds);           // Convert to ML standard dataset format (ML.Types.NumericField).
//OUTPUT(full_ds, NAMED('full_ds'), ALL);
indepData:= full_ds(number<5);   
depData:= PROJECT(full_ds(number=5),TRANSFORM(Types.DiscreteField, SELF.number:=1, SELF:=LEFT));// dependent data must be discrete

//Set RandomForest to generate a random forest of 25 trees selecting 3 features for splits using impurity:=1.0 and max depth:= 5
learner := Classify.RandomForest(25, 3, 1.0, 5);
ClassifyingModel := learner.learnc(IndepData, DepData); // model to use when classifying
OUTPUT(ClassifyingModel,NAMED('ClassifyingModel'), ALL); // group_id represent number of tree
ReadableModel:= learner.modelC(ClassifyingModel);  // transforming model to a easier way to read it
OUTPUT(SORT(ReadableModel, group_id, node_id),NAMED('ReadableModel'), ALL); // group_id represent number of tree

//Class distribution for each Instance
ClassDist:= learner.ClassProbDistribC(IndepData, ClassifyingModel);
OUTPUT(ClassDist, NAMED('ClassDist'), ALL);
class:= learner.classifyC(IndepData, ClassifyingModel); // classifying
OUTPUT(class, NAMED('class_result'), ALL); // conf show voting percentage

//Measuring Performance of Classifier
performance:= Classify.Compare(depData, class);
OUTPUT(performance.CrossAssignments, NAMED('CrossAssig'));
OUTPUT(performance.RecallByClass, NAMED('RecallByClass'));
OUTPUT(performance.PrecisionByClass, NAMED('PrecisionByClass'));
OUTPUT(performance.FP_Rate_ByClass, NAMED('FP_Rate_ByClass'));// False Positive Rate

//Calculating Area Under the Curve for each Class
AUC1:= Classify.AUC_ROC(ClassDist, 1, depData); //Area under ROC Curve for class "1"
OUTPUT(AUC1, ALL, NAMED('AUC_1'));
AUC2:= Classify.AUC_ROC(ClassDist, 2, depData); //Area under ROC Curve for class "2"
OUTPUT(AUC2, ALL, NAMED('AUC_2'));
AUC3:= Classify.AUC_ROC(ClassDist, 3, depData); //Area under ROC Curve for class "3"
OUTPUT(AUC3, ALL, NAMED('AUC_3'));
