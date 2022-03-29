% build the train data set and test data set
function [images_train,labels_train,images_test,labels_test] = buildtrainset(sizetrain,iftrain)
    if iftrain==true
        total_labels = importdata('E:/文档/MATLAB/labels.txt');
        N = size(total_labels,1);
        images = cell(1,3*N);
        labels=categorical(reshape(total_labels',1,3*N));
        for k = 1:N
            im = imread(sprintf('E:/文档/MATLAB/imagedata/train_%04d.png', k));
            cutimages = cutting(im,false);
            images{3*k-2} = cutimages{1};
            images{3*k-1} = cutimages{2};
            images{3*k  } = cutimages{3};
        end
        
        images4darray = zeros(64,64,1,3*N);
        for j = 1:3*N
            images4darray(:,:,1,j)=images{j};
        end

        images_train=images4darray(:,:,1,1:3*sizetrain);
        labels_train=labels(1:3*sizetrain);
        images_test =images4darray(:,:,1,3*sizetrain+1:end);
        labels_test =labels(3*sizetrain+1:end);
    end    
end