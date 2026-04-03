% read Pill data from raw csv file
% step1: separate numbers of pills
% step2: data clean for each pill
% step3: sync pill into 1 timetable
% Example df_all = f_readpill('abc.csv')

% Version: SBS.0.0.1

% Date 24 Jan 2023
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




function df_output = f_readpill(fn)
plt = 1;
% determine number of pill
df = readtable(fn, ...
    'VariableNamingRule', 'preserve',...
    'Range','A10');
df = df(1:find(df.Var1==0)-1,:); % remove header info starting from 0
df.Properties.VariableNames{2} = 'time';

% writetable(df,'SBS_379_pill_V8.csv')
%% Separate pill data

% read data for each pill
st_pill = find(df.Var1==1);
N_pill = length(st_pill);
DF = [];

for i=1:N_pill
    if i==N_pill
        DF{i} = df(st_pill(i):end, [2 i*2+1]);
    else
        DF{i} = df(st_pill(i):st_pill(i+1)-1, [2 i*2+1]);
    end
end

%% Data cleaning 

DF_timetable = [];
for ii=1:length(DF)
    df_i = DF{1,ii};
    %df_i.Properties.VariableNames{3} = 'Temperature';
    
    cbt = df_i.(2);
    %plot(df_i.Temperature); hold on
    
    % remove cbt drop and increase 0.4 oC per minute (0.2 per 30 seconds)
    % 10 minutes before and after will be removed
    t_thres = 0.2;
    time_thres = 30;
    id = find(abs( diff( cbt )) >= t_thres);
    
    rm_id_1 = [];
    for i=1:length(id)
        id_i = id(i)-time_thres*2 :  id(i)+time_thres*2;
        rm_id_1 = [rm_id_1, id_i];
    end
    
    % remove data <35 and >38.5 oC
    rm_id_2 = find(cbt<=35 | cbt>=38.5);
    
    
    % combine all remove index
    rm_id = [rm_id_1, rm_id_2'];
    rm_id = unique(rm_id); % remove possible overlaping range
    id = (rm_id>=1 & rm_id<=height(df_i)); % negative index and max length
    rm_id = rm_id(id);
    
    
    df_i.(2)(rm_id) = NaN; % artifact == nan
    df_i.time = df_i.time + years(2000);
    %plot(df_i.Temperature,'--')
    
    DF_timetable{1,ii} = table2timetable(df_i);
end

clear DF df_i rm_id rm_id_1 rm_id_2
%% synchronize all pills


df_all = synchronize(DF_timetable{1,1},...
    'regular','median','TimeStep',seconds(30));

% df_all = synchronize(DF_timetable{1,1},...
%     DF_timetable{1,2},...
%     'regular','median','TimeStep',seconds(30));

if N_pill>1
    for i=2:N_pill
        df_all = synchronize(df_all,...
                 DF_timetable{1,i},...
                'regular','median','TimeStep',seconds(30));
    end
end

cbt_array = [];
for i=1:N_pill
    cbt_array = [cbt_array, df_all.(i)];
end


df_all.meanCBT = mean(cbt_array,2,'omitnan');

% writetimetable(df_all, '379_all_temp.csv')

%% final output

df_output = df_all(:, {'meanCBT'});

%df_output = df_all(:,4); %4 could be 14?


%% Validation plots
if plt==1
    figure
    for i=1:N_pill
        col_index = 2 * i + 1;  % Adjust this formula based on your pattern
plot(df.(2)+years(2000), df.(col_index), 'Color',[floor(rand*100),floor(rand*100),189]/255); hold on
% plot(df.(2)+years(2000), df.(5), 'Color',[230,85,13]/255);
% plot(df.(2)+years(2000), df.(7), 'Color',[117,107,177]/255);
    end


plot(df_output.time, df_output.meanCBT,'--', 'Color',[99,99,99]/255,...
    'LineWidth', 1.5)
xlabel('Date time'),
ylabel('Core body temperature, ^oC')
legend('Raw pill data', '','','Clean data')
title('Sync and clean CBT')
end

end
