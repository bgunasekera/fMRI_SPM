%Extract contrast estimates from SPM.mat file 

roi_files = spm_get(Inf,'*roi.mat', 'Select ROI files'); %This will extract raw time course from all ROI.mat files in the directory
des_path = spm_get(1, 'SPM.mat', 'Select SPM.mat');
rois = maroi('load_cell', roi_files); % make maroi ROI objects
des = mardo(des_path);  % make mardo design object
mY = get_marsy(rois{:}, des, 'mean'); % extract data into marsy data object
y  = summary_data(mY);  % get summary time course(s)