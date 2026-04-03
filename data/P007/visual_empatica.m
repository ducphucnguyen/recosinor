clc, clear, close

% Specify the directory
fd = ['R:\CMPH-NHMRC Temperature Grant\8. Study Data & Analyses\Pilot data\' ...
    'HSTest_Empatica\1w1a326k10e1y5a1_8-11-24'];

fd = ['v2']
% Get a list of all CSV files in the folder and its subfolders
filePattern = fullfile(fd, '**', '*.csv'); % '**' means search subfolders
csvFiles = dir(filePattern);

% Display the names of the files

sensors = {'accelerometers-std', 'pulse-rate', 'temperature'};

Combinedsensor = {};

for i=1:length(sensors)
temperatureFiles = csvFiles(contains({csvFiles.name}, sensors{i}));

% Read files
% Initialize an empty table to hold combined data
combinedData = table();

for k = 1:length(temperatureFiles)
    filePath = fullfile(temperatureFiles(k).folder, temperatureFiles(k).name);
    tempData = readtable(filePath); % Read the file into a table
    tempData.missing_value_reason = string(tempData.missing_value_reason);
    combinedData = [combinedData; tempData]; % Concatenate tables
end

% convert to datetime
combinedData.datetime = datetime(combinedData.timestamp_unix/1000, 'ConvertFrom', 'posixtime');
combinedData.datetime = combinedData.datetime + hours(10) + minutes(30);

% plot for each sensor
combinedTimetable = table2timetable(combinedData);

Combinedsensor{i} = combinedTimetable;

end

stackedplot(Combinedsensor{1, 1}(:,4:end), ...
    Combinedsensor{1, 2}(:,4:end),...
    Combinedsensor{1, 3}(:,4:end)); 



%%

% Extract the relevant data
datetime_col = Combinedsensor{1,2}.datetime;
pulse_col = Combinedsensor{1,2}.pulse_rate_bpm;

% Create a new table
T = table(datetime_col, pulse_col, 'VariableNames', {'Datetime', 'PulseRate'});


% Extract the relevant data
datetime_col = Combinedsensor{1,1}.datetime;
accel_col = Combinedsensor{1,1}.accelerometers_std_g;

% Create a new table
T_accel = table(datetime_col, accel_col, 'VariableNames', {'Datetime', 'AccelStdG'});


% Extract the relevant data
datetime_col = Combinedsensor{1,3}.datetime;
temp_col = Combinedsensor{1,3}.temperature_celsius;

% Create a new table
T_temp = table(datetime_col, temp_col, 'VariableNames', {'Datetime', 'TemperatureC'});

% Optional: display first few rows

% Make sure all tables use the same datetime column from the first table
df_empatica = table(T.Datetime, T.PulseRate, T_accel.AccelStdG, T_temp.TemperatureC, ...
                    'VariableNames', {'Datetime', 'PulseRate', 'AccelStdG', 'TemperatureC'});

% Optional: display first few rows
writetable(df_empatica,'df_empatica.csv')










