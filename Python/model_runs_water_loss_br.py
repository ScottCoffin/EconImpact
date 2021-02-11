import pandas as pd
import numpy as np
import win32com.client
import sys
sys.path.append('C:/ProgramData/Anaconda3/pkgs')#/openpyxl-3.0.5-py_0/site-packages')
import openpyxl
import os
from datetime import datetime

#Put new (supplier-specific) parameters into the model and refresh the spreadsheet
# New values need to be extracted
def input_and_refresh(model_work,mod_refresh,test_inputs):
    #Use openpyxl to overwrite the model inputs
    wbk_mod=openpyxl.load_workbook(model_work)
    #The syntax is workbook[Sheet Name][Cell name]
    wbk_mod['Inputs']['B9'].value = test_inputs['Current real loss (AF)']
    wbk_mod['Inputs']['B10'].value = test_inputs['Length of mains']
    wbk_mod['Inputs']['B11'].value = test_inputs['Num connections']
    wbk_mod['Inputs']['B12'].value = test_inputs['Variable production cost (AF)']
    wbk_mod['Inputs']['B13'].value = test_inputs['Avg operating pressure']
    #Save the model file
    wbk_mod.save(model_work)
    del wbk_mod
    #Use win32com to open and refresh the model
    #If you don't do this, the values won't update!
    xlapp = win32com.client.DispatchEx("Excel.Application")
    xlapp.DisplayAlerts = False
    wb = xlapp.Workbooks.Open(model_work)
    wb.RefreshAll()
    #Save as new, individualized output model for supplier
    xlapp.ActiveWorkbook.SaveAs(mod_refresh)
    xlapp.ActiveWorkbook.Save()
    xlapp.Quit()
    del wb

#Extract the output values from an output supplier-specific model file
def extract_output_values(xl):
    #Open the file as read only for the extraction
    wb=openpyxl.load_workbook(xl,read_only=True,data_only=True)
    #Create the dictionary to store the values
    dict_store={}
    #Extract the desired outputs
    outsheet=wb['Output']
    dict_store['Current leakage (gpc)']=outsheet['D8'].value
    dict_store['Standard (gpc)']=outsheet['D9'].value
    dict_store['Current leakage (gpm)']=outsheet['E8'].value
    dict_store['Standard (gpm)']=outsheet['E9'].value
    dict_store['Benefit-Cost-30yr']=outsheet['G7'].value
    dict_store['Benefit-Cost-20yr']=outsheet['G10'].value
    dict_store['Benefit-Cost-10yr']=outsheet['G13'].value
    dict_store['Benefit-Cost-05yr']=outsheet['G16'].value
    #Extract the inputs for reference
    insheet=wb['Inputs']
    dict_store['Average real loss']=insheet['B9'].value
    dict_store['Length of mains']=insheet['B10'].value
    dict_store['Number of service connections']=insheet['B11'].value
    dict_store['Variable production cost']=insheet['B12'].value
    dict_store['Average operational pressure']=insheet['B13'].value
    dict_store['Unreported leakage fraction']=insheet['B33'].value
    wb.close()
    del wb
    return(dict_store)

#Get the values from the calculations tab for doing sums
def extract_sums(xl):
    #Open using pandas (it's faster!!)
    letters=[chr(i) for i in range(ord('A'),ord('Z')+1)]
    numbers=[i for i in range(0,len(letters))]
    ldict = {letters[i]: numbers[i] for i in range(len(letters))} 
    
    cols=['A','I','J','M','O','P','R','T','U']
    colnames=['Timestep since 2022','Water saved (I, sum)',
              'Present value of leak detection (J, sum, mod)',
              'Present value of leak repair costs (M, sum, mod)',
              'Present value of leak detection and repair cost (O, sum)',
             'Marginal cost of water (P, mean)',
              'Present value of water loss reduced (R, sum)',
             'Present value of net benefit (T, sum)','Year']
    ns=[ldict[c] for c in cols]
    #open the file as read only
    colvals=pd.read_excel(xl,skiprows=range(0,71),usecols=ns,sheet_name='Calculations',header=None)
    colvals.columns=colnames
    colvals=colvals.iloc[0:360,:]
    jv=colvals['Present value of leak detection (J, sum, mod)']
    mv=colvals['Present value of leak repair costs (M, sum, mod)']
    tv=colvals['Timestep since 2022']
    #This is an adjustment that needed to be done for a reason I forget
    colvals.loc[:,'Present value of leak detection (J, sum, mod)']=jv/((1+(0.035/12.))**(24+tv-1))
    colvals.loc[:,'Present value of leak repair costs (M, sum, mod)']=mv/((1+(0.035/12.))**(24+tv-1))
    return(colvals)

#Specifications for running the model:
#Current (script) directory
dirn=os.getcwd()

#Model version
vn='V4_2021_02-10' #'21Aug2020_V3_Alt'
#Model file name
#Probably will want to put the full path... just in case!
model_work='SWRCB_WaterLossModel_5January2021_V4.xlsx'
#Read the file that contains the inputs for the various suppliers (extracted from water loss audits)
inputs=pd.read_excel('Model_auditinputs_2020_07_06.xlsx')

#These are the output column names of the overall summed/mean values
# NOTE:
# Make sure these match the names of the columns in the extract_sums function if you change anything
colnames_sum=['Present value of leak detection (J, sum, mod)',
          'Present value of leak repair costs (M, sum, mod)',
         'Marginal cost of water (P, mean)',
         'Present value of net benefit (T, sum)',
         'Present value of leak detection and repair cost (O, sum)',
        'Present value of water loss reduced (R, sum)',
          'Water saved (I, sum)']
#These are the output column names of the per-year summed/mean values
# NOTE:
# Make sure these match the names of the columns in the extract_sums function if you change anything
colnames_year=['Water saved (I, sum)','Present value of leak detection and repair cost (O, sum)',
              'Marginal cost of water (P, mean)','Present value of water loss reduced (R, sum)']

#These are the output column names of the standards and inputs for the suppliers
# NOTE:
# Make sure these match the names of the columns in the extract_output_values function if you change anything
colnames_standards=['Current leakage (gpc)','Current leakage (gpm)','Standard (gpc)','Standard (gpm)',
                   'Benefit-Cost-30yr','Benefit-Cost-20yr','Benefit-Cost-10yr','Benefit-Cost-05yr',
                   'Average real loss','Length of mains','Number of service connections',
                   'Variable production cost','Average operational pressure','Unreported leakage fraction']

#Where the individual model runs will be stored
#PROBABLY WANT TO CHANGE THIS TO A LOCAL DIRECTORY
output_subdir= r'{:}/{:}'.format(dirn,vn) # r'S:\EXECUTIVE\OSI\Water Loss\MODEL\{:}'.format(vn)
if not os.path.exists(output_subdir):
    os.makedirs(output_subdir)

#Where the summary files will be stored
#PROBABLY WANT TO CHANGE THIS TO A LOCAL DIRECTORY
sub_summ_dir=r'{:}/{:}/Summaries'.format(dirn,vn) #r'S:\EXECUTIVE\OSI\Water Loss\MODEL\Summaries\{:}'.format(vn)
if not os.path.exists(sub_summ_dir):
    os.makedirs(sub_summ_dir)

#This is the name of the model file that will be used as the intermediate step in the calculations
#I don't use the original file, just in case something gets fucked
mod_refresh=r'{:}/SWRCB_WaterLossModel_5January2021_V4_copy.xlsx'.format(dirn)   

#The data frames that will store the various values   
data_store=pd.DataFrame()
data_sum=pd.DataFrame()
data_year=pd.DataFrame()

for i in range(0,len(inputs)):
    #Supplier specific inputs from the input file
    test_inputs=inputs.iloc[i,:]
    sn=test_inputs.loc['Supplier Name']
    #Replace special characters, otherwise you won't be able to write an output file
    sn=sn.replace("/","-")
    snf=sn.replace(" ","")
    #Name of the supplier-specific output file (change to whatever you want)
    mod_refresh_rev=r'{:}/model_calculations_{:}_2020_02_10_V4.xlsx'.format(output_subdir,snf)
    print(mod_refresh_rev)
    #Write the supplier-specific model file
    input_and_refresh(model_work,mod_refresh_rev,test_inputs)

    #Initiate the dictionary for storing values
    dict_values={'Supplier name':sn}
    #Extract the output values
    dict_store=extract_output_values(mod_refresh_rev)
    #Copy over to the dictionary that contains the supplier name
    for k in colnames_standards:
        dict_values[k]=dict_store[k]
    #Write a new row to the data frame
    data_store=data_store.append(dict_values,ignore_index=True)

    #Do the extraction template
    #Overall sum
    df_extract=extract_sums(mod_refresh_rev)
    df_sum_all=df_extract.agg({colnames_sum[0]:'sum',colnames_sum[1]:'sum',colnames_sum[2]:'mean',
                              colnames_sum[3]:'sum',colnames_sum[4]:'sum',colnames_sum[5]:'sum',
                              colnames_sum[6]:'sum'})
    df_sum_all.name = sn
    data_sum=data_sum.append(df_sum_all)
    #Yearly sum
    df_sum_year=df_extract.groupby('Year').agg({colnames_year[0]:'sum',colnames_year[1]:'sum',
                                                colnames_year[2]:'mean',colnames_year[3]:'sum'})
    df_sum_year.name = sn
    df_t=df_sum_year.transpose()
    d = {}
    for y in df_t.columns:
        d[y]=pd.DataFrame(df_t[y]).transpose().rename(index={y:sn})
    year_arr=pd.concat(d,axis=1)
    data_year = data_year.append(year_arr)

#Reorganize the columns in the output dataframe
dat_order=['Supplier name']+colnames_standards 

#Output summary file for all suppliers
w=pd.ExcelWriter('{:}/model_summary_standards_2020_02_10.xlsx'.format(sub_summ_dir))
data_sum.to_excel(w,'Lifecycle')
data_year.to_excel(w,'Yearly')
data_store[dat_order].to_excel(w,'Standards',index=False)
w.save()
w.close()