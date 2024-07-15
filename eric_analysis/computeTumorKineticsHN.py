"""
For a Head and Neck project specifically, this script tracks the amount of air inside
a contoured airbox and GTV throughout CBCTs as well as other metric.

@author:  leeje
"""

import connect
import clr, System, os
clr.AddReference("System.Windows.Forms")
clr.AddReference("System.Drawing")
from System.Windows.Forms import (Application, Form)
from System.Windows.Forms import (ComboBox, ComboBoxStyle, Button, Label, ListBox,
                                  SelectionMode, FolderBrowserDialog, OpenFileDialog,
                                  RadioButton, GroupBox)
from System.Drawing import Size

parentCurrentPath = os.path.abspath(os.path.join(os.getcwd(), os.pardir))
connect.append_path(parentCurrentPath)

def checkForInconsistency(myPatient, isRigidReg):
    """
    Run at the start of the code, to check for any possible cases where the rest of the code would fail
    @param myPatient:
    @return:
    """
    # %% check if GTV and airbox exists, if not give error
    roiNames = [roi.Name for roi in myPatient.PatientModel.RegionsOfInterest]

    if 'GTV' not in roiNames:
        raise Exception('GTV in not on Planning CT')
    if 'airbox' not in roiNames:
        raise Exception('airbox is not available on Planning CT')
    if 'EXTERNAL' not in roiNames:
        raise Exception('EXTERNAL is not available')

    tempScriptROIs = ['GTV+5mm', 'airbox-expanded', 'airInCurROIBoundingBox', 'air-450-in-airbox',
                      'air-450-in-GTV', 'air-450-in-GTV+5mm', 'airbox-to-skin', 'airbox-at-skin', 'airboxTemp', 'airboxTemp2']
    for tempROI in tempScriptROIs:
        if tempROI in roiNames:
            myPatient.PatientModel.RegionsOfInterest[tempROI].DeleteRoi()

    ExamName = [exam.Name for exam in myPatient.Examinations]
    if 'Planning CT' not in ExamName:
        raise Exception('There is no image named Planning CT.')

    # Use rigid registration.  Don't need to check here.
    if not isRigidReg:
         imgRegGroup = [group.Name for group in myPatient.PatientModel.StructureRegistrationGroups]
         if len(imgRegGroup)>1:
             raise Exception('There are more than one registration group.')
         elif len(imgRegGroup) == 0:
             raise Exception('No deformation image registration is performed.  Can not deformably mapped airbox to CBCTs')


def expandGTV(myPatient, myExamList, isCT):
    """ margin expansion of GTV with 5mm"""
    for myExam in myExamList:
        if isCT:
            retval_0 = myPatient.PatientModel.CreateRoi(Name="GTV+5mm", Color="SaddleBrown", Type="Organ",
                                                        TissueName=None, RoiMaterial=None)
            retval_0.SetMarginExpression(SourceRoiName='GTV',
                                         MarginSettings={'Type': "Expand", 'Superior': 0,
                                                               'Inferior': 0, 'Anterior': 0.5,
                                                               'Posterior': 0.5, 'Right': 0.5,
                                                               'Left': 0.5})
        else:
            retval_0 = myPatient.PatientModel.RegionsOfInterest['GTV+5mm']
        retval_0.UpdateDerivedGeometry(Examination=myExam, Algorithm="Auto")


def contourAirInSelectRoi(myExamList, mypatient, ROIStudiedList, isCT):
    """
    Contour air for each of the ROI in ROIStudiedList
    @param myExamList:
    @param mypatient:
    @param ROIStudiedList:
    @param isCT:
    @return:
    """

    airInROIList = []

    retval_0 = mypatient.PatientModel.CreateRoi(Name="airInCurROIBoundingBox", Color="Cyan", Type="Organ",
                                                TissueName=None, RoiMaterial=None)
    for curRoi in ROIStudiedList:
        for myExam in myExamList:
            # create a bounding box around curROI
            BoundingBox = mypatient.PatientModel.StructureSets[myExam.Name].RoiGeometries[curRoi].GetBoundingBox()

            BoundingBoxDict = {"MinCorner": {'x': BoundingBox[0].x, 'y': BoundingBox[0].y, 'z': BoundingBox[0].z},
                               'MaxCorner': {'x': BoundingBox[1].x, 'y': BoundingBox[1].y, 'z': BoundingBox[1].z}}

            # use threshold to find air inside curROI's bounding box
            retval_0.GrayLevelThreshold(Examination=myExam, LowThreshold=-2048, HighThreshold=-450, PetUnit="",
                                        BoundingBox=BoundingBoxDict)

            # create air in current ROI by intersect between airInCurROIBoundingBox and curRoi
            airInCurRoi = "air-450-in-" + curRoi
            if isCT:
                retval_1 = mypatient.PatientModel.CreateRoi(Name=airInCurRoi, Color="Pink", Type="Organ", TissueName=None,
                                                            RoiMaterial=None)
            else:
                retval_1 = mypatient.PatientModel.RegionsOfInterest[airInCurRoi]

            retval_1.SetAlgebraExpression(
                ExpressionA={'Operation': "Intersection", 'SourceRoiNames': [curRoi, "airInCurROIBoundingBox"],
                             'MarginSettings': {'Type': "Expand", 'Superior': 0, 'Inferior': 0,
                                                'Anterior': 0, 'Posterior': 0, 'Right': 0,
                                                'Left': 0}},
                ExpressionB={'Operation': "Union", 'SourceRoiNames': [],
                             'MarginSettings': {'Type': "Expand", 'Superior': 0, 'Inferior': 0,
                                                'Anterior': 0, 'Posterior': 0, 'Right': 0,
                                                'Left': 0}},
                ResultOperation="None",
                ResultMarginSettings={'Type': "Expand", 'Superior': 0, 'Inferior': 0,
                                      'Anterior': 0, 'Posterior': 0, 'Right': 0, 'Left': 0})
            retval_1.UpdateDerivedGeometry(Examination=myExam, Algorithm="Auto")

        airInROIList.append(airInCurRoi)

    # delete the curROI's bounding box
    mypatient.PatientModel.RegionsOfInterest["airInCurROIBoundingBox"].DeleteRoi()

    return airInROIList


def computeImageIntensityStats(myExam, roiName):
    import math
    if myExam.EquipmentInfo.Modality == 'Cbct':
        isCBCT = False
    else:
        isCBCT = False

    histogram = myExam.Series[0].ImageStack.GetImageStackHistogram(BinSize=10, ExcludePixelPadding=False,
                                                                   RoiName=roiName,
                                                                   ConvertCbctValueToHu=False)

    # histogram = myExam.Series[0].ImageStack.GetIntensityStatistics(ExcludePixelPadding=False, RoiName=roiName,
    #                                                              ConvertCbctValueToHu=False)
    sumGreyLevels = 0.0
    nrEl = 0
    maxIntensity = -float('inf')
    minIntensity = float('inf')
    for curIntensity in histogram:
        nrEl = nrEl + curIntensity.Value
        sumGreyLevels = sumGreyLevels + curIntensity.Value * curIntensity.Key
        if curIntensity.Key > maxIntensity and curIntensity.Value != 0:
            maxIntensity = float(curIntensity.Key)
        if curIntensity.Key < minIntensity and curIntensity.Value != 0:
            minIntensity = float(curIntensity.Key)
    if nrEl != 0:
        meanIntensity = sumGreyLevels / float(nrEl)
        meanIntensityStr = '%.2f' % (sumGreyLevels / float(nrEl))
        stdsum = 0.0
        for key in histogram.Keys:
            stdsum = stdsum + math.pow(key - meanIntensity, 2) * histogram[key]
        stdIntensity = '%.2f' % (math.sqrt(stdsum / nrEl))
        maxIntensity = '%.2f' % maxIntensity
        minIntensity = '%.2f' % minIntensity
    else:
        meanIntensityStr = 'NA'
        stdIntensity = 'NA'
        maxIntensity = 'NA'
        minIntensity = 'NA'
    statsDict = {'Mean': meanIntensityStr, 'StDev': stdIntensity, 'Max': maxIntensity, 'Min': minIntensity}
    return statsDict


def get_sortedCBCTs(myPatient, CTExam):
    # get a list of CBCT sorted by their examination date
    import datetime
    print 'get_sortedCBCTs!!'
    AllExamsDate = []

    # date of CT exam
    CTDateObj = CTExam.GetExaminationDateTime()
    CTDate = datetime.datetime(CTDateObj.Year, CTDateObj.Month, CTDateObj.Day, CTDateObj.Hour,
                               CTDateObj.Minute, CTDateObj.Second)
    AllExamsDate.append(CTDate.date())

    cbcts = [ex for ex in myPatient.Examinations if 'CBCT' in ex.EquipmentInfo.Modality.upper() and ex.ImportFraction > 0]
    assert len(cbcts) > 0, "No CBCTs found"
    cbcts.sort(key=lambda x: x.GetExaminationDateTime())
    numFractions = 35

    cbctsFinal = []
    fx = 1
    while fx <= numFractions:
        if len(cbcts) > 0:
            print cbcts[0].ImportFraction
            if cbcts[0].ImportFraction == fx:
                cbctdt = cbcts[0].GetExaminationDateTime()
                cbctdt = datetime.datetime(cbctdt.Year, cbctdt.Month, cbctdt.Day, cbctdt.Hour, cbctdt.Minute, cbctdt.Second)
                AllExamsDate.append(cbctdt.date())
                cbctsFinal.append(cbcts.pop(0))
                fx = fx + 1
            elif cbcts[0].ImportFraction == 0:
                # not needed fraction
                cbcts.pop(0)
            elif cbcts[0].ImportFraction > fx:
                cbctsFinal.append(None)
                AllExamsDate.append('missing')
                fx = fx + 1
        else:
            cbctsFinal.append(None)
            AllExamsDate.append('missing')
            fx = fx + 1

    for i, x in enumerate(cbctsFinal):
        if x is not None:
            print x.Name
        else:
            print 'None'


    return cbctsFinal, AllExamsDate


def computeStatsOnSelectRoi(myPatient, myExamList, myROINameList):
    metricDict = {}
    for myExam in myExamList:
        tempDict = {}
        for curRoiName in myROINameList:
            roiGeometry = myPatient.PatientModel.StructureSets[myExam.Name].RoiGeometries[curRoiName]
            if roiGeometry.HasContours():
                #IntensityStatsDict = computeImageIntensityStats(myExam, curRoiName)
                #tempDict[curRoiName] = {'Volume': '%.2f' % vol, 'Intensity': IntensityStatsDict}
                vol = roiGeometry.GetRoiVolume()
                print "{} has volume in {}".format(curRoiName, myExam)
            else:
                vol = 0
                print "{} do not have volume in {}".format(curRoiName, myExam)
            tempDict[curRoiName] = {'Volume': '%.2f' % vol}
        metricDict[myExam.Name] = tempDict

    return metricDict


def computeAirboxDimension(mypatient, CTExam):
    # compute the dimension of airbox
    boundingBox = mypatient.PatientModel.StructureSets[CTExam.Name].RoiGeometries["airbox"].GetBoundingBox()

    LeftRight = boundingBox[0].x - boundingBox[1].x
    AntPost = boundingBox[0].y - boundingBox[1].y
    SupInf = boundingBox[0].z - boundingBox[1].z

    outDict = {}
    outDict["airbox dimension"] = {'LR': '%.2f' % LeftRight, 'AP': '%.2f' % AntPost, 'SI': '%.2f' % SupInf}
    return outDict



def analyzeAirboxToSkin(mypatient, myExamList, AirBoxDimDict, myLogger):
    # assume an roi name 'airbox' and 'External' already exist

    airboxAPLength = AirBoxDimDict["airbox dimension"]['AP']
    airboxSILength = AirBoxDimDict["airbox dimension"]['SI']


    # create ROI airbox-to-skin
    # expand airbox left-right by 15 cm, to create 'airbox-expanded'
    retval_0 = mypatient.PatientModel.CreateRoi(Name="airbox-expanded", Color="SaddleBrown", Type="Organ",
                                                TissueName=None, RoiMaterial=None)

    # intersection of airbox-expanded and External, to create 'airbox-to-skin'
    retval_3 = mypatient.PatientModel.CreateRoi(Name="airbox-to-skin", Color="Pink", Type="Organ", TissueName=None,
                                                RoiMaterial=None)


    outDict = {}
    for exam in myExamList:
        AirBoxCenter = mypatient.PatientModel.StructureSets[exam.Name].RoiGeometries['airbox'].GetCenterOfRoi()

        retval_0.CreateBoxGeometry(Size={'x': 30, 'y': airboxAPLength, 'z': airboxSILength}, Examination=exam,
                                   Center=AirBoxCenter)

        retval_3.CreateAlgebraGeometry(Examination=exam, Algorithm="Auto",
                                       ExpressionA = {'Operation': "Intersection", 'SourceRoiNames': ["airbox-expanded", "EXTERNAL"],
                                                      'MarginSettings': {'Type': "Expand", 'Superior': 0, 'Inferior': 0,
                                                                         'Anterior': 0, 'Posterior': 0, 'Right': 0,
                                                                         'Left': 0}},
                                       ExpressionB = {'Operation': "Union", 'SourceRoiNames': [],
                                                      'MarginSettings': {'Type': "Expand", 'Superior': 0, 'Inferior': 0,
                                                                         'Anterior': 0, 'Posterior': 0, 'Right': 0,
                                                                         'Left': 0}},
                                       ResultOperation="None",
                                       ResultMarginSettings={'Type': "Expand", 'Superior': 0, 'Inferior': 0,
                                                             'Anterior': 0, 'Posterior': 0, 'Right': 0, 'Left': 0})

        airboxToSkinGeo = mypatient.PatientModel.StructureSets[exam.Name].RoiGeometries["airbox-to-skin"]

        if not airboxToSkinGeo.HasContours():
            raise Exception('airbox-to-Skin has no contours')

        # compute the volume of airbox-to-skin
        vol_airboxToSkin = airboxToSkinGeo.GetRoiVolume()

        outDict[exam.Name] = {"airbox-to-skin": {'Volume': '%.2f' % vol_airboxToSkin}}


    #mypatient.PatientModel.RegionsOfInterest['airbox-expanded'].DeleteRoi()

    return outDict


def analyzeAirboxToSkin2(mypatient, myExamList, myLogger=None):

    # assume an roi name 'airbox' and 'External' already exist

    # create ROI airbox-to-skin
    # expand airbox left-right by 15 cm, to create 'airbox-expanded'
    # retval_0 = mypatient.PatientModel.CreateRoi(Name="airboxTemp", Color="SaddleBrown", Type="Organ",
    #                                             TissueName=None, RoiMaterial=None)
    # retval_1 = mypatient.PatientModel.CreateRoi(Name="airboxTemp2", Color="SaddleBrown", Type="Organ",
    #                                             TissueName=None, RoiMaterial=None)
    # # intersection of airbox-expanded and External, to create 'airbox-to-skin'
    retval_3 = mypatient.PatientModel.CreateRoi(Name="airbox-to-skin", Color="Pink", Type="Organ", TissueName=None,
                                                RoiMaterial=None)

    #outDict = {}
    for exam in myExamList:
        # retval_0.CreateMarginGeometry(Examination=exam, SourceRoiName='airbox',
        #                               MarginSettings={'Type': "Expand", 'Superior': 0, 'Inferior': 0,
        #                                                 'Anterior': 0, 'Posterior': 0, 'Right': 5, 'Left': 5})
        #
        # retval_1.CreateMarginGeometry(Examination=exam, SourceRoiName='airboxTemp',
        #                               MarginSettings={'Type': "Expand", 'Superior': 0, 'Inferior': 0,
        #                                               'Anterior': 0, 'Posterior': 0, 'Right': 5, 'Left': 5})
        if myLogger:
            myLogger.print_w_time('debug exam name {}'.format(exam.Name))

        retval_3.CreateAlgebraGeometry(Examination=exam, Algorithm="Auto",
                                       ExpressionA={'Operation': "Union",
                                                    'SourceRoiNames': ["airbox"],
                                                    'MarginSettings': {'Type': "Expand", 'Superior': 0, 'Inferior': 0,
                                                                       'Anterior': 0, 'Posterior': 0, 'Right': 15,
                                                                       'Left': 15}},
                                       ExpressionB={'Operation': "Union", 'SourceRoiNames': ['EXTERNAL'],
                                                    'MarginSettings': {'Type': "Expand", 'Superior': 0, 'Inferior': 0,
                                                                       'Anterior': 0, 'Posterior': 0, 'Right': 0,
                                                                       'Left': 0}},
                                       ResultOperation="Intersection",
                                       ResultMarginSettings={'Type': "Expand", 'Superior': 0, 'Inferior': 0,
                                                             'Anterior': 0, 'Posterior': 0, 'Right': 0, 'Left': 0})
        if myLogger:
            myLogger.print_w_time('done algebra geo')

        airboxToSkinGeo = mypatient.PatientModel.StructureSets[exam.Name].RoiGeometries["airbox-to-skin"]

        if not airboxToSkinGeo.HasContours():
            raise Exception('airbox-to-Skin has no contours')

        # compute the volume of airbox-to-skin
        vol_airboxToSkin = airboxToSkinGeo.GetRoiVolume()

        if myLogger:
            myLogger.print_w_time('got volume')

        #outDict[exam.Name] = {"airbox-to-skin": {'Volume': '%.2f' % vol_airboxToSkin}}


    #mypatient.PatientModel.RegionsOfInterest['airboxTemp'].DeleteRoi()
    #mypatient.PatientModel.RegionsOfInterest['airboxTemp2'].DeleteRoi()

    return outDict


def writeGeometryToExcel(myROINameList, ExamRoiMetricDict, CTExam, cbcts, allExamsDate, xlsFile, myMRN):

    clr.AddReference("Microsoft.Office.Interop.Excel")
    import Microsoft.Office.Interop.Excel as Excel
    from System import Array

    # set the column name MRN, MetricName, CT, CBCTs
    numColumns = len(cbcts) + 3
    ColName = Array.CreateInstance(object, len(cbcts))
    for i, cbct in enumerate(cbcts):
        if cbct is not None:
            ColName[i] = cbct.Name
        else:
            ColName[i] = 'CBCT Fx {}'.format(i+1)

    # check if the xls file already exist
    myExcel = Excel.ApplicationClass(Visible=False, DisplayAlerts=False)
    if os.path.isfile(xlsFile):
        # if file already exist
        myWorkbook = myExcel.Workbooks.Open(xlsFile)
    else:
        myWorkbook = myExcel.Workbooks.Add()
    allExistingSheet = [sheet.Name for sheet in myWorkbook.Sheets]
    try:
        # check if the sheet already exist for each ROI
        # if not, add a row of column names
        for roiName in myROINameList:
            for metric in ExamRoiMetricDict[CTExam.Name][roiName].keys():
                if metric not in ['Intensity']:
                    SheetName = metric + '_' + roiName
                    SheetName = SheetName[-31:]
                    if SheetName in allExistingSheet:
                        mySheet = myWorkbook.Sheets[SheetName]
                    else:
                        mySheet = myWorkbook.Sheets.Add()
                        mySheet.Name = SheetName
                        mySheet.Cells[1, 1] = "Patient MRN"
                        mySheet.Cells[1, 2] = "Metric (cc)"
                        mySheet.Cells[1, 3] = "CT"
                        myCell1 = mySheet.Cells(1, 4)
                        myCell2 = mySheet.Cells(1, 4 + len(cbcts) - 1)
                        mySheet.Range(myCell1, myCell2).Value = ColName

                    outNetArray = Array.CreateInstance(object, len(cbcts) + 3)
                    outNetArray[0] = myMRN
                    outNetArray[1] = metric
                    outNetArray[2] = ExamRoiMetricDict[CTExam.Name][roiName][metric]
                    for i, cbct in enumerate(cbcts):
                        if cbct is not None:
                            outNetArray[3 + i] = ExamRoiMetricDict[cbct.Name][roiName][metric]
                        else:
                            outNetArray[3 + i] = ' '
                    addRowIndex = mySheet.UsedRange.Rows.Count + 1
                    myCell1 = mySheet.Cells(addRowIndex, 1)
                    myCell2 = mySheet.Cells(addRowIndex, 1 + numColumns - 1)
                    mySheet.Range(myCell1, myCell2).Value = outNetArray

        # add a sheet for airbox dimension
        SheetName = "airbox_dim"
        if SheetName in allExistingSheet:
            mySheet = myWorkbook.Sheets[SheetName]
        else:
            mySheet = myWorkbook.Sheets.Add()
            mySheet.Name = SheetName
            mySheet.Cells[1, 1] = "Patient MRN"
            mySheet.Cells[1, 2] = "Metric"
            mySheet.Cells[1, 3] = "LR [cm]"
            mySheet.Cells[1, 4] = "AP [cm]"
            mySheet.Cells[1, 5] = "SI [cm]"
        outNetArray = Array.CreateInstance(object, 5)
        outNetArray[0] = myMRN
        outNetArray[1] = 'airbox dimension on CT'
        outNetArray[2] = ExamRoiMetricDict[CTExam.Name]['airbox dimension']['LR']
        outNetArray[3] = ExamRoiMetricDict[CTExam.Name]['airbox dimension']['AP']
        outNetArray[4] = ExamRoiMetricDict[CTExam.Name]['airbox dimension']['SI']
        addRowIndex = mySheet.UsedRange.Rows.Count + 1
        myCell1 = mySheet.Cells(addRowIndex, 1)
        myCell2 = mySheet.Cells(addRowIndex, 1 + 5 - 1)
        mySheet.Range(myCell1, myCell2).Value = outNetArray

        # add a sheet for the dates
        SheetName = "Dates"
        if SheetName in allExistingSheet:
            mySheet = myWorkbook.Sheets[SheetName]
        else:
            mySheet = myWorkbook.Sheets.Add()
            mySheet.Name = SheetName
            mySheet.Cells[1, 1] = "Patient MRN"
            mySheet.Cells[1, 2] = "Metric"
            mySheet.Cells[1, 3] = "CT"
            myCell1 = mySheet.Cells(1, 4)
            myCell2 = mySheet.Cells(1, 4 + len(cbcts) - 1)
            mySheet.Range(myCell1, myCell2).Value = ColName
        outNetArray = Array.CreateInstance(object, len(cbcts) + 3)
        outNetArray[0] = myMRN
        outNetArray[1] = 'Dates'
        outNetArray[2] = str(allExamsDate[0])
        for i in range(len(cbcts)):
            outNetArray[3 + i] = str(allExamsDate[i+1])
        addRowIndex = mySheet.UsedRange.Rows.Count + 1
        myCell1 = mySheet.Cells(addRowIndex, 1)
        myCell2 = mySheet.Cells(addRowIndex, 1 + numColumns - 1)
        mySheet.Range(myCell1, myCell2).Value = outNetArray
    finally:
        myWorkbook.SaveAs(xlsFile)
        myWorkbook.Close()
        myExcel.Quit()


def writeIntensityToExcel(myROINameList, ExamRoiMetricDict, CTExam, cbcts, allExamsDate, xlsFile, myMRN):
    clr.AddReference("Microsoft.Office.Interop.Excel")
    import Microsoft.Office.Interop.Excel as Excel
    from System import Array

    # set the column name MRN, MetricName, CT, CBCTs
    numColumns = len(cbcts) + 3
    ColName = Array.CreateInstance(object, len(cbcts))
    for i, cbct in enumerate(cbcts):
        if cbct is not None:
            ColName[i] = cbct.Name
        else:
            ColName[i] = 'CBCT Fx {}'.format(i)

    # check if the xls file already exist
    myExcel = Excel.ApplicationClass(Visible=False, DisplayAlerts=False)
    if os.path.isfile(xlsFile):
        # if file already exist
        myWorkbook = myExcel.Workbooks.Open(xlsFile)
    else:
        myWorkbook = myExcel.Workbooks.Add()
    allExistingSheet = [sheet.Name for sheet in myWorkbook.Sheets]

    try:
        # check if the sheet already exist for each ROI
        # if not, add a row of column names
        for roiName in myROINameList:
            metric = 'Intensity'
            if metric in ExamRoiMetricDict[CTExam.Name][roiName].keys():
                SheetName = metric + '_' + roiName
                SheetName = SheetName[-31:]
                if SheetName in allExistingSheet:
                    mySheet = myWorkbook.Sheets[SheetName]
                else:
                    mySheet = myWorkbook.Sheets.Add()
                    mySheet.Name = SheetName
                    mySheet.Cells[1, 1] = "Patient MRN"
                    mySheet.Cells[1, 2] = "Metric"
                    mySheet.Cells[1, 3] = "CT"
                    myCell1 = mySheet.Cells(1, 4)
                    myCell2 = mySheet.Cells(1, 4 + len(cbcts) - 1)
                    mySheet.Range(myCell1, myCell2).Value = ColName

                numRow = len(ExamRoiMetricDict[CTExam.Name][roiName][metric])
                listIntensityMetric = ExamRoiMetricDict[CTExam.Name][roiName][metric].keys()
                numIntenstiyMetric = len(listIntensityMetric)
                outNetArray = Array.CreateInstance(object, numIntenstiyMetric, len(cbcts) + 3)
                outNetArray[0, 0] = myMRN
                for i, k in enumerate(listIntensityMetric):
                    outNetArray[i, 1] = k
                for i, k in enumerate(listIntensityMetric):
                    outNetArray[i, 2] = ExamRoiMetricDict[CTExam.Name][roiName][metric][k]
                for i, cbct in enumerate(cbcts):
                    for j, k in enumerate(listIntensityMetric):
                        if cbct is not None:
                            outNetArray[j, i + 3] = ExamRoiMetricDict[cbct.Name][roiName][metric][k]
                        else:
                            outNetArray[j, i + 3] = ' '
                addRowIndex = mySheet.UsedRange.Rows.Count + 1
                myCell1 = mySheet.Cells(addRowIndex, 1)
                myCell2 = mySheet.Cells(addRowIndex + numRow - 1, 1 + numColumns - 1)
                mySheet.Range(myCell1, myCell2).Value = outNetArray

        # add a sheet for the dates
        SheetName = 'Dates'
        if SheetName in allExistingSheet:
            mySheet = myWorkbook[SheetName]
        else:
            mySheet = myWorkbook.Sheets.Add()
            mySheet.Name = SheetName
            mySheet.Cells[1, 1] = "Patient MRN"
            mySheet.Cells[1, 2] = "Metric"
            mySheet.Cells[1, 3] = "CT"
            myCell1 = mySheet.Cells(1, 4)
            myCell2 = mySheet.Cells(1, 4 + len(cbcts) - 1)
            mySheet.Range(myCell1, myCell2).Value = ColName
        outNetArray = Array.CreateInstance(object, len(cbcts) + 3)
        outNetArray[0] = myMRN
        outNetArray[1] = 'Dates'
        outNetArray[2] = str(allExamsDate[0])
        for i in range(len(cbcts)):
            outNetArray[3 + i] = str(allExamsDate[i+1])
        addRowIndex = mySheet.UsedRange.Rows.Count + 1
        myCell1 = mySheet.Cells(addRowIndex, 1)
        myCell2 = mySheet.Cells(addRowIndex, 1 + numColumns - 1)
        mySheet.Range(myCell1, myCell2).Value = outNetArray
    finally:
        myWorkbook.SaveAs(xlsFile)
        myWorkbook.Close()
        myExcel.Quit()


def mapGTVAirboxToCBCT(myPatient, CTExam, cbcts, isRigidReg, myLogger = None):

    if isRigidReg:
        # rigidly map GTV and airbox to cbct
        try:
            cbctsName = [cbct.Name for cbct in cbcts]
            myPatient.PatientModel.CopyRoiGeometries(SourceExamination=CTExam, TargetExaminationNames=cbctsName,
                                                     RoiNames=["GTV", "airbox"])
        except Exception, e:
            raise Exception('Error in rigidly mapping GTV and airbox to CBCTs.  {}'.format(e.message))
    else:
        # deformably map GTV and airbox to cbct
        RegGroupName = myPatient.PatientModel.StructureRegistrationGroups[0].Name
        print RegGroupName
        ROI_to_map = ['airbox', 'GTV']
        for roiName in ROI_to_map:
            myCbcts = [cbct.Name for cbct in cbcts if cbct is not None and
                       not myPatient.PatientModel.StructureSets[cbct.Name].RoiGeometries[roiName].HasContours()]
            if len(myCbcts)>0:
                myRefExams = ["Planning CT"] * len(myCbcts)
                mySregNames = [RegGroupName] * len(myCbcts)
                try:
                    myPatient.MapRoiGeometriesDeformably(RoiGeometryNames=[roiName], CreateNewRois=False,
                                                       StructureRegistrationGroupNames=mySregNames,
                                                       ReferenceExaminationNames=myRefExams,
                                                       TargetExaminationNames=myCbcts, ReverseMapping=False,
                                                       AbortWhenBadDisplacementField=False)
                except:
                    Exception('Error in DIR mapping airbox or GTV onto CBCT ')
            else:
                print '{} is not mapped to CBCT. myCBCTs is {}'.format(roiName, myCbcts)
                if myLogger:
                    myLogger.print_w_time('{} is not mapped to CBCT. myCBCTs is {}'.format(roiName, myCbcts))




def main(curpatient, xlsFileGeo, xlsFileIntensity, isRigidReg, myLogger=None):
    import Logger
    import datetime

    # check if ROIs needed for the study is missing on planning CT
    checkForInconsistency(curpatient, isRigidReg)

    PlanCTName = 'Planning CT'
    CTExam = curpatient.Examinations[PlanCTName]

    # a list of cbct sorted by their examination date
    cbcts, AllExamsDate = get_sortedCBCTs(curpatient, CTExam)
    cbctsNonNone = [cbct for cbct in cbcts if cbct is not None]
    examsToProcess = [CTExam] + cbctsNonNone

    mapGTVAirboxToCBCT(curpatient, CTExam, cbctsNonNone, isRigidReg, myLogger)
    # AirboxtToSkinVolDict = analyzeAirboxToSkin2(curpatient, examsToProcess, myLogger)
    curpatient.Save()

    # process for CT
    # expand GTV and airbox to form GTV+5mm and airbox-expanded
    expandGTV(curpatient, [CTExam], isCT=True)
    # create the ROI that contains the air in GTV, GTV+5mm and airbox
    selectROIList = ['airbox', 'GTV', 'GTV+5mm']
    airInROIList = contourAirInSelectRoi([CTExam], curpatient, selectROIList, isCT=True)
    # compute statistics for [airbox, GTV, GTV+5mm, air-450-in-airbox, air-450-in-GTV, air-450-in-GTV+5mm]
    ROINameList = selectROIList + airInROIList

    # process on each CBCT
    expandGTV(curpatient, cbctsNonNone, isCT=False)
    contourAirInSelectRoi(cbctsNonNone, curpatient, selectROIList, isCT=False)

    ExamRoiMetricDict = computeStatsOnSelectRoi(curpatient, examsToProcess, ROINameList)
    tempDict = {}
    tempDict = computeAirboxDimension(curpatient, CTExam)
    ExamRoiMetricDict[CTExam.Name].update(tempDict)

    # this operation does not work.  Omit for now.
    #AirboxtToSkinVolDict = analyzeAirboxToSkin2(curpatient, examsToProcess, myLogger)
    #for ex in examsToProcess:
    #    ExamRoiMetricDict[ex.Name].update(AirboxtToSkinVolDict[ex.Name])


    MRN = curpatient.PatientID
    #WriteROINameList = ROINameList + ['airbox-to-skin']
    WriteROINameList = ROINameList
    if myLogger:
        myLogger.print_w_time('Write out data to excel.')

    writeGeometryToExcel(WriteROINameList, ExamRoiMetricDict, CTExam, cbcts, AllExamsDate, xlsFileGeo, MRN)
    #writeIntensityToExcel(WriteROINameList, ExamRoiMetricDict, CTExam, cbcts, AllExamsDate, xlsFileIntensity, MRN)



class myForm(Form):

    def __init__(self):
        self.Width = 500
        self.Height = 500
        self.Text = 'Tumour Kinetic Study for Head & Neck'
        self.CenterToScreen()

        formMargin = 20
        Gap = 25

        # label for describing the usage
        label0 = Label(Top=20, Left=20)
        label0.Parent = self
        label0.MaximumSize = Size(480, 0)
        label0.Text = 'Compute the volume of air inside GTV, GTV+5mm and airbox for CT and CBCTs.  ' \
                      'GTV and airbox are rigidly or deformably mapped from CT to CBCTs.'
        label0.AutoSize = True

        gb = GroupBox(Top=0.25*self.Height, Left=formMargin)
        gb.Text = 'Select a method for mapping GTV and airbox from CT to CBCTs:'
        gb.Size = Size(self.Width-2*formMargin, 110)
        gb.Parent = self

        RigidBT = RadioButton(Top= formMargin, Left=formMargin)
        RigidBT.Text = "Rigid"
        RigidBT.Parent = gb
        RigidBT.CheckedChanged += self.RadioOnChanged

        DeformBT = RadioButton(Top = RigidBT.Top+Gap, Left=formMargin)
        DeformBT.Text = "Deformable"
        DeformBT.Parent = gb
        DeformBT.CheckedChanged += self.RadioOnChanged


        label4 = Label(Top=290, Left=20, Width=480)
        label4.Parent = self
        label4.Text = 'Select an output folder:'
        bt = Button(Top=320, Left=20)
        bt.Text = 'Browse...'
        bt.Parent = self
        bt.Click += self.FolderBrowserDialogClick

        # text to display selected path
        self.label5 = Label(Top=320, Left=100, Width=480)
        self.label5.Parent = self
        self.userSelectedPath = ''
        self.label5.Text = self.userSelectedPath
        self.label5.MaximumSize = Size(480, 0)
        self.label5.AutoSize = True

        # OK button
        self.bt1 = Button(Top=400, Left=20)
        self.bt1.Parent = self
        self.bt1.Text = "OK"
        self.bt1.Click += self.clickok

        self.msgText = Label(Top=400, Left=100, Width=550, Height=40)
        self.msgText.Parent = self
        self.msgText.MaximumSize = Size(480, 0)
        self.msgText.AutoSize = True
        self.msgText.Text = ''

    def RadioOnChanged(self, sender, event):
        if sender.Checked:
            self.RegMethod = sender.Text

    def FolderBrowserDialogClick(self, sender, event):
        folderDialog = FolderBrowserDialog()
        folderDialog.SelectedPath = r'M:'
        folderDialog.Description = 'Select an output folder:'
        folderDialog.ShowNewFolderButton = True
        if folderDialog.ShowDialog() == System.Windows.Forms.DialogResult.OK:
            self.userSelectedPath = folderDialog.SelectedPath
            self.label5.Text = self.userSelectedPath


    def clickok(self, sender, event):
        import time
        import datetime
        import traceback

        self.bt1.Enabled = False
        patient = connect.get_current("Patient")
        if len(self.userSelectedPath) == 0:
            self.msgText.Text = 'Please select a path.'
        elif len(self.RegMethod) == 0:
            self.msgText.Text = 'Please select a registration method.'
        else:
            if self.RegMethod == 'Rigid':
                isRigigReg = True
            else:
                isRigigReg = False

            self.msgText.Text = 'Running'
            try:
                start = time.time()
                filenameGeo = 'HN_TumorKineticGeometric.xlsx'
                filenameIntensity = 'HN_TumorKineticIntensity.xlsx'
                xlsFileGeo = os.path.join(self.userSelectedPath, filenameGeo)
                xlsFileIntensity = os.path.join(self.userSelectedPath, filenameIntensity)
                today = datetime.datetime.now().strftime('%m%d%H%M')
                if os.path.isfile(xlsFileGeo):
                    xlsFileGeo = os.path.splitext(xlsFileGeo)[0] + '_' + self.RegMethod + today + '.xlsx'
                if os.path.isfile(xlsFileIntensity):
                    xlsFileIntensity = os.path.splitext(xlsFileIntensity)[0] + '_' + self.RegMethod + today + '.xlsx'
                main(patient, xlsFileGeo, xlsFileIntensity, isRigigReg, myLogger=None)
                end = time.time()
                elaspedTime = int((end - start) / 60)
                self.msgText.Text = 'Completed: {}\n Elapsed Time: {:d} mins.'.format(xlsFileGeo, elaspedTime)
            except Exception, e:
                self.msgText.Text = 'Error.  {}'.format(e.message)
                traceback.print_exc()
        self.bt1.Enabled = True


if __name__ == "__main__":

    aForm = myForm()
    Application.Run(aForm)

