% id2 = fopen('lena2.raw', 'r')
% x2 = fread( id2, [256,256], 'uint8')
% 
% id1 = fopen('lena_conv_single.raw', 'r')
% conv_single = fread( id1, [256,256], 'uint8')
% 
% id0 = fopen('lena.raw', 'r')
% orig = fread( id0, [256,256], 'uint8')
% 
% id4 = fopen('lena4.raw', 'r')
% x4 = fread( id4, [256,256], 'uint8')

id5 = fopen('lena_conv_single.raw', 'r')
conv_single = fread( id5, [256,256], 'uint8')

% id6 = fopen('lena_convolved_out.raw', 'r')
% conv_double_out = fread( id6, [256,256], 'uint8')
colormap('gray')
imagesc(conv_single)