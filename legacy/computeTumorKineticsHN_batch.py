"""
For a Head and Neck project specifically, this script tracks the amount of air inside
a contoured airbox and GTV throughout CBCTs as well as other metric.

@author:  leeje
"""

import connect
import clr, System, os, time
clr.AddReference("System.Windows.Forms")
from System.Windows.Forms import (Application, Form)
from System.Windows.Forms import (ComboBox, ComboBoxStyle, Button, Label, ListBox,
                                  SelectionMode, FolderBrowserDialog, OpenFileDialog)
clr.AddReference("System.Drawing")
from System.Drawing import Size

parentCurrentPath = os.path.abspath(os.path.join(os.getcwd(), os.pardir))
connect.append_path(parentCurrentPath)


def batchMain(BatchPtsJsonFile, userSelectedPath):
    import batchProcessModule, Logger
    import computeTumorKineticsHN
    import time
    import datetime

    today = datetime.datetime.now().strftime('%m%d%H%M')

    # create a logger
    LoggerFile = 'BatchLogger_' + today + '.txt'
    BatchLoggerFile = os.path.join(userSelectedPath, LoggerFile)
    if os.path.isfile(BatchLoggerFile):
        # the log file already exists.  Delete it.
        os.remove(BatchLoggerFile)
    BatchLogger = Logger.Logger(BatchLoggerFile)

    filenameGeo = 'HN_TumorKineticGeometric.xlsx'
    filenameIntensity = 'HN_TumorKineticIntensity.xlsx'
    ExcelFileGeo = os.path.join(userSelectedPath, filenameGeo)
    ExcelFileIntensity = os.path.join(userSelectedPath, filenameIntensity)
    ExcelFileGeo = os.path.splitext(ExcelFileGeo)[0] + '_' + today + '.xlsx'
    ExcelFileIntensity = os.path.splitext(ExcelFileIntensity)[0] + '_' + today + '.xlsx'

    mainArgDict = {'xlsFileGeo': ExcelFileGeo, 'xlsFileIntensity': ExcelFileIntensity, 'myLogger': BatchLogger}

    batchProcessModule.batchMain(BatchPtsJsonFile, computeTumorKineticsHN.main, BatchLogger, **mainArgDict)



class myForm(Form):

    def __init__(self):
        self.Width = 500
        self.Height = 500
        self.Text = 'Tumour Kinetic Study for Head & Neck (Batch)'
        self.CenterToScreen()

        # label for describing the usage
        label0 = Label(Top=20, Left=20)
        label0.Parent = self
        label0.MaximumSize = Size(480, 0)
        label0.Text = 'Compute the volume of air inside GTV, GTV+5mm and airbox for CT and CBCTs.  ' \
                      'GTV and airbox are rigidly mapped from CT to CBCTs.'
        label0.AutoSize = True

        buttonStart = 250
        label4 = Label(Top=buttonStart, Left=20, Width=480)
        label4.Parent = self
        label4.Text = 'Select an output folder:'
        bt = Button(Top=buttonStart+25, Left=20)
        bt.Text = 'Browse...'
        bt.Parent = self
        bt.Click += self.FolderBrowserDialogClick

        # text to display selected path
        self.label5 = Label(Top=buttonStart+25, Left=100, Width=480)
        self.label5.Parent = self
        self.userSelectedPath = ''
        self.label5.Text = self.userSelectedPath
        self.label5.MaximumSize = Size(480, 0)
        self.label5.AutoSize = True

        # button for selecting a file
        buttonStart = 325
        label6 = Label(Top=buttonStart, Left=20, Width=480)
        label6.Parent = self
        label6.Text = 'Select the json file specifying a list of patients:'
        bt1 = Button(Top=buttonStart+25, Left=20)
        bt1.Text = 'Browse...'
        bt1.Parent = self
        bt1.Click += self.FileBrowserDialogClick

        # display json file
        self.label7 = Label(Top=buttonStart+25, Left=100, Width=480)
        self.label7.Parent = self
        self.userBatchJsonFile = ''
        self.label7.Text = self.userBatchJsonFile

        # OK button
        self.bt1 = Button(Top=420, Left=20)
        self.bt1.Parent = self
        self.bt1.Text = "OK"
        self.bt1.Click += self.clickok

        self.msgText = Label(Top=400, Left=100, Width=550, Height=40)
        self.msgText.Parent = self
        self.msgText.MaximumSize = Size(450, 0)
        self.msgText.AutoSize = True
        self.msgText.Text = ''

    def FolderBrowserDialogClick(self, sender, event):
        folderDialog = FolderBrowserDialog()
        folderDialog.ShowNewFolderButton = True
        folderDialog.SelectedPath = r'M:'
        folderDialog.Description = "Please select a folder for export."
        if (folderDialog.ShowDialog() == System.Windows.Forms.DialogResult.OK):
            self.userSelectedPath = folderDialog.SelectedPath
            self.label5.Text = self.userSelectedPath


    def FileBrowserDialogClick(self, sender, event):
        fileDialog = OpenFileDialog()
        fileDialog.Filter = "json files (*.json)|*.json"
        if fileDialog.ShowDialog(self) == System.Windows.Forms.DialogResult.OK:
            self.userBatchJsonFile = fileDialog.FileName
            self.label7.Text = self.userBatchJsonFile


    def clickok(self, sender, event):


        self.bt1.Enabled = False
        if len(self.userSelectedPath) == 0:
            self.msgText.Text = 'Please select a output path.'
        elif len(self.userBatchJsonFile) == 0:
            self.msgText.Text = 'Please select a json file.'
        else:
            self.msgText.Text = 'Running'
            start = time.time()
            batchMain(self.userBatchJsonFile, self.userSelectedPath)
            end = time.time()
            elaspedTime = int((end - start) / 60)
            self.msgText.Text = 'Completed.\n Elapsed Time is {:d} mins.'.format(elaspedTime)
        self.bt1.Enabled = True


if __name__ == "__main__":

    aForm = myForm()
    Application.Run(aForm)

