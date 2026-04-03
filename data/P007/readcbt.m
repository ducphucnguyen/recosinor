
clc, clear, close

fn = '2025-02-06T13-08-59-P007.csv';

df_all = f_readpill(fn);

writetimetable(df_all,'cbt.csv')