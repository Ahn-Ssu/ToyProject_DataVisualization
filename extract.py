import numpy as np
import matplotlib.pyplot as plt
import pandas as pd 

import os
os.environ['CUDA_VISIBLE_DEVICES'] = '1'



for i in range(20200201,20200229):
    index = i 
    index = str(index)
    print("start", index)
    pfPath = "./2020dataPackage/pf/02-concatH/"
    df = pd.read_csv(pfPath+"concatH_202001.csv",encoding='ISO-8859-1')
    df = df[df['base_ymd'].isin([index])]
    df.to_csv("./0101data_"+index+".csv", encoding='ISO-8859-1')
    print("end")
