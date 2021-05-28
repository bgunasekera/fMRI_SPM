%creating Marsbar ROI mask


imgname = 'sV1.nii'; %Change this to .nii image of choice
o = maroi_image(struct('vol', spm_vol(imgname), 'binarize',0,...
'func', 'img'));
o = maroi_matrix(o);
saveroi(o, 'my_image_roi.mat') %Change this to name of choice- ideally to match imgname