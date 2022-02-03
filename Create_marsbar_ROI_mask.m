%creating Marsbar ROI mask


imgname = 'sV1.nii'; %Change this to .nii image of choice
o = maroi_image(struct('vol', spm_vol(imgname), 'binarize',0,...
'func', 'img'));
o = maroi_matrix(o);
saveroi(o, 'my_image_roi.mat') %Change this to name of choice- ideally to match imgname

%%% NOTE %%%
% To convert something like an atlas (i.e Desikan-Killiany) into ROI.mat file can be done very easily using the "import" option in Marsbar gui.
% The atlas is a single number-labelled image. 
% Atlas must be unzipped: Use

gunzip filename

% The single atlas .nii file will be convered into 83 ROI.mat files (in the case of DK)
% Would be wise to make a specialised directory for it before hand and include the relevant csv file denoting the number to brain regions
% I have already done this for DK. In my home direcotry there is a file named "DK_parcellation.