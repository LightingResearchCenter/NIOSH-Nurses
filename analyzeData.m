function analyzeData
%ANALYZEDATA Summary of this function goes here
%   Detailed explanation goes here
timestamp = datestr(now,'yyyy-mm-dd HH-MM');

[githubDir,~,~] = fileparts(pwd);
d12packDir = fullfile(githubDir,'d12pack');
addpath(d12packDir);

projectDir = '\\root\projects\NIOSH_RedLightForShiftWorkers\daysimeter_data';
% projectDir = '/Users/geoff/Desktop/Acuity MtSinai';
dataDir = fullfile(projectDir,'croppedData');
saveDir = fullfile(projectDir,'tables');

ls = dir([dataDir,filesep,'*.mat']);
% [~,idxMostRecent] = max(vertcat(ls.datenum));

for iFile = 1:numel(ls)
    dataName = ls(iFile).name;
    dataPath = fullfile(dataDir,dataName);
    
    saveName = regexprep(dataName,'\.mat','');
    
    load(dataPath);
    
    nObj = numel(objArray);
    h = waitbar(0,'Please wait. Analyzing data...');
    rn1 = datestr(datetime(0,0,0,0,0,0):duration(1,0,0):datetime(0,0,0,23,0,0),'HH:MM - ');
    rn2 = datestr(datetime(0,0,0,1,0,0):duration(1,0,0):datetime(0,0,0,24,0,0),'HH:MM');
    RowNames = cellstr([rn1,rn2]);
    RowNames = [RowNames;{'Mean'}];
    
    for iObj = 1:nObj
        
        obj = objArray(iObj);
        
        if isa(obj,'d12pack.HumanData')
            idxKeep = obj.Observation & obj.Compliance & ~obj.Error & ~obj.InBed;
        else
            idxKeep = obj.Observation & ~obj.Error;
        end
        
        t = obj.Time(idxKeep);
        ai = obj.ActivityIndex(idxKeep);
        lux = obj.Illuminance(idxKeep);
        cla = obj.CircadianLight(idxKeep);
        cs = obj.CircadianStimulus(idxKeep);
        
        try
        date0 = dateshift(t(1),'start','day');
        catch
            continue
        end
        dateF = dateshift(t(end),'start','day');
        dates = date0:calendarDuration(0,0,1):dateF;
        
        nDates = numel(dates);
        tb = array2table(nan(25,nDates));
        tb.Properties.VariableNames = cellstr(datestr(dates,'mmm_dd_yyyy'));
        tb.Properties.RowNames = RowNames;
        
        aiTB  = tb;
        luxTB = tb;
        claTB = tb;
        csTB  = tb;
        coverageTB = tb;
        
        aiTB.Properties.DimensionNames{1} = 'Activity Index';
        luxTB.Properties.DimensionNames{1} = 'Illuminance';
        claTB.Properties.DimensionNames{1} = 'Circadian Light';
        csTB.Properties.DimensionNames{1} = 'Circadian Stimulus';
        coverageTB.Properties.DimensionNames{1} = '# of Samples';
        coverageTB.Properties.RowNames{25} = 'Total';
        
        for iCol = 1:nDates
            for iRow = 1:24
                idx = t >= (dates(iCol)+duration(iRow-1,0,0)) & t < (dates(iCol)+duration(iRow,0,0));
                
                if any(idx)
                    aiTB{iRow,iCol}  = mean(ai(idx));
                    luxTB{iRow,iCol} = mean(lux(idx));
                    claTB{iRow,iCol} = mean(cla(idx));
                    csTB{iRow,iCol}  = mean(cs(idx));
                end
                
                coverageTB{iRow,iCol} = sum(idx);
            end
            
            idx = t >= dates(iCol) & t < (dates(iCol)+duration(24,0,0));
            aiTB{25,iCol}  = mean(ai(idx));
            luxTB{25,iCol} = mean(lux(idx));
            claTB{25,iCol} = mean(cla(idx));
            csTB{25,iCol}  = mean(cs(idx));
            coverageTB{25,iCol} = sum(idx);
        end
        
        
        sheet = obj.ID;
        
        aiName = [saveName,' Mean AI','.xlsx'];
        aiPath = fullfile(saveDir,aiName);
        writetable(aiTB,aiPath,'Sheet',sheet,'WriteVariableNames',true,'WriteRowNames',true);
        
        luxName = [saveName,' Mean Lux','.xlsx'];
        luxPath = fullfile(saveDir,luxName);
        writetable(luxTB,luxPath,'Sheet',sheet,'WriteVariableNames',true,'WriteRowNames',true);
        
        claName = [saveName,' Mean CLA','.xlsx'];
        claPath = fullfile(saveDir,claName);
        writetable(claTB,claPath,'Sheet',sheet,'WriteVariableNames',true,'WriteRowNames',true);
        
        csName = [saveName,' Mean CS','.xlsx'];
        csPath = fullfile(saveDir,csName);
        writetable(csTB,csPath,'Sheet',sheet,'WriteVariableNames',true,'WriteRowNames',true);
        
        coverageName = [saveName,' Analysis Coverage','.xlsx'];
        coveragePath = fullfile(saveDir,coverageName);
        writetable(coverageTB,coveragePath,'Sheet',sheet,'WriteVariableNames',true,'WriteRowNames',true);
        
        waitbar(iObj/nObj);
    end
    close(h);
    
end

end

