import numpy as np
import matplotlib.pyplot as plt
import pandas as pd 

import os
os.environ['CUDA_VISIBLE_DEVICES'] = '1'

print("start")



locationPath = "./2020dataPackage/location/"

locationH = pd.read_excel(locationPath+"KIKcd_H.20181210.xlsx",sheet_name="KIKcd_H")

H_code = locationH['행정동코드']
H_codeArray = H_code.to_numpy()

print(H_codeArray)

for indexNumber, Hcode in zip(range(0,len(H_codeArray)), H_codeArray):
    temp = Hcode
    temp = str(temp)
    temp = temp[0:4]
    H_codeArray[indexNumber] = int(temp)
print(H_codeArray)

locationH['행정동코드']=H_codeArray

print(locationH)

locationH.to_csv("./slicH.csv", encoding='euc-kr')




print("end")