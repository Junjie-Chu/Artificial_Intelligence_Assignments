% % cut images
function y = cutting(img, isshow)
	if nargin < 2; isshow = false; end
    
	if isshow
		imshow(img); % show images
    end
    
	thresh = graythresh(img); 
	BW = 1 - imbinarize(img,thresh);  
	I1 = bwareaopen(BW, 100, 8);	 % remove small areas
    
    SE=strel('rectangle',[3 3]);%Set a structure
    g1 = imopen(I1,SE);%'open' can erode the image to make objects
    g2=g1;
    if isshow
        figure;imshow(g2),title('Eroded image')
    end
    
    I2=g2;
	varray = sum(I2); 
	imgsize = size(I2);

	if isshow
		harray = sum(I2');
		x1 = 1 : imgsize(1, 1);
		x2 = 1 : imgsize(1, 2);
		figure; % show the image after cut
		plot(x1, harray, 'r+-', x2, varray, 'y*-');

		figure; 
		imshow(I2); % show the image
	end

	va = mean(varray);   % compute the average(threhold)
	harray = sum(I2'); 
	vb = mean(harray);
	
	isanum = false; 
	sumy = 0;
	for i = 1 : imgsize(1,1)
		if harray(i) > vb
			if isanum == false
				isanum = true;
				cvb = i;
			end
		else
			if isanum
				isanum = false;
				cve = i;
				sumy = sumy + 1;
				if isshow
					hold on;
					plot([0 imgsize(1,2)], [cvb cvb],'r--');%plot cut lines
					plot([0 imgsize(1,2)], [cve cve], 'r--');
				end
			end
		end
	end

	y = cell(1,3);
	sumy = length(y);
    ctbs = [];
    sumb = 0;
    ctes = [];
    sume = 0;
	for i = 1 : imgsize(1, 2)
		if varray(i) > va
			if isanum == false
				isanum = true;
                sumb=sumb+1;
				ctbs(sumb) = i;
			end
		else
			if isanum == true
				isanum = false;
				cte = i;
                sume=sume+1;
				ctes(sume) = i;
			
            end
        end
    end
    if isshow
        hold on;
		plot([ctbs(1) ctbs(1)], [0 imgsize(1,1)],'r--');
		plot([ctbs(1)+30 ctbs(1)+30], [0 imgsize(1,1)],'r--');
        plot([ctes(length(ctes))-30 ctes(length(ctes))-30], [0 imgsize(1,1)],'r--');
		plot([ctes(length(ctes)) ctes(length(ctes))], [0 imgsize(1,1)],'r--');
    end
    
    t1 = I2(cvb:cve, ctbs(1):ctbs(1)+30);% first number
    t2 = I2(cvb:cve, ctbs(1)+29:ctes(length(ctes))-30);% second number
    t3 = I2(cvb:cve, ctes(length(ctes))-29:ctes(length(ctes)));%third number
    SE1= strel('rectangle',[1 3]);
    t1 = imopen(t1,SE1);
    t1 = bwareaopen(t1, 10, 8);	 
    t2 = imopen(t2,SE1);
    t2 = bwareaopen(t2, 10, 8);	 
    t3 = imopen(t3,SE1);
    t3 = bwareaopen(t3, 10, 8);	 
    
    max_size = [64 64];
    
	y{1} = t1;
    y{2} = t2;
    y{3} = t3;
    
    for j = 1 : 3
		temp = zeros(max_size);
		imgs_size = size(y{j});
		temp(1:imgs_size(1,1), 1:imgs_size(1,2)) = y{j};
		y{j} = temp;
    end
    
    if isshow
        figure;
        subplot(1,3,1);
        imshow(y{1});
        subplot(1,3,2);
        imshow(y{2});
        subplot(1,3,3);
        imshow(y{3});
    end
end


