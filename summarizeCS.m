function summarizeCS
%SUMMARIZECS Summary of this function goes here
%   Detailed explanation goes here

% Create timestamp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
timestamp = datestr(now,'yyyy-mm-dd_HHMM');
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



% Enable dependencies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[githubDir,~,~] = fileparts(pwd);
d12packDir = fullfile(githubDir,'d12pack');
addpath(d12packDir);
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



% Map project folder paths ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
projectDir = '\\root\projects\NIOSH_RedLightForShiftWorkers\daysimeter_data';

dataDir  = fullfile(projectDir,'croppedData');
saveDir  = fullfile(projectDir,'tables');
saveName = [timestamp,' Average CS summary','.xlsx'];
savePath = fullfile(saveDir,saveName);

if exist(saveDir, 'dir') == 0
    mkdir(saveDir);
end
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cropLS = dir([dataDir,filesep,'*.mat']);
% Find newested cropped file
[~,idxMax] = max([cropLS.datenum]);
dataPath  = fullfile(dataDir, cropLS(idxMax).name);

sheet = regexprep(cropLS(idxMax).name,'\.mat','');

load(dataPath);

nObj = numel(objArray);
h = waitbar(0,'Please wait. Analyzing data...');

tb = table;
tb.mean_valid_CS = nan(nObj,1);
tb.category = cell(nObj,1);
tb.Properties.RowNames = regexprep({objArray.ID}','(\d\d\d).*','$1');
tb.Properties.DimensionNames{1} = ['file_',sheet];

for iObj = 1:nObj
    obj = objArray(iObj);
    
    idxValid = obj.Observation & ~obj.InBed & obj.Compliance & ~obj.Error;
    
    tb.mean_valid_CS(iObj)  = mean(obj.CircadianStimulus(idxValid));
    
    if tb.mean_valid_CS(iObj) < 0.1
        tb.category{iObj} = 'CS < 0.1';
    elseif tb.mean_valid_CS(iObj) >= 0.1 && tb.mean_valid_CS(iObj) < 0.2
        tb.category{iObj} = ['0.1 ',char(8804),' CS < 0.2'];
    elseif tb.mean_valid_CS(iObj) >= 0.2 && tb.mean_valid_CS(iObj) < 0.3
        tb.category{iObj} = ['0.2 ',char(8804),' CS < 0.3'];
    elseif tb.mean_valid_CS(iObj) >= 0.3
        tb.category{iObj} = ['CS ',char(8805),' 0.3'];
    end
    
    waitbar(iObj/nObj);
end
writetable(tb,savePath,'Sheet',sheet,'WriteVariableNames',true,'WriteRowNames',true);
close(h);

end

