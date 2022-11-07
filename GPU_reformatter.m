clear all;close all;clc;

%% Header
%This script loads and converts csv files into a shape easier for me to
%analyze with R. No statistical analysis is performed.

%% Filepaths
filepath = "C:\Users\clark.pehrson\Documents\";
Crypto_factors_filename = "DIM_CRYPTO_DATA.csv";
GPU_factors_filename = "DIM_GPU_PROD.csv";
Merchant_factors_filename = "DIM_MERCHANT.csv";
Region_factors_filename = "DIM_REGION.csv";
Time_factors_filename = "DIM_TIME.csv";
GPU_price_filename = "FACT_GPU_PRICE.csv";
Crypto_price_filename = "FACT_CRYPTO_RATE.csv";


%% Loading in Data
Crypto_factors      = readtable(fullfile(filepath,Crypto_factors_filename));
GPU_factors         = readtable(fullfile(filepath,GPU_factors_filename));
Merchant_factors    = readtable(fullfile(filepath,Merchant_factors_filename));
Region_factors      = readtable(fullfile(filepath,Region_factors_filename));
Time_factors        = readtable(fullfile(filepath,Time_factors_filename));
GPU_price           = readtable(fullfile(filepath,GPU_price_filename));
Crypto_price        = readtable(fullfile(filepath,Crypto_price_filename));

%% Only pass through data with enough points (gpus, sellers, regions)
    %gpu types
        gpu_count = zeros(height(GPU_factors),1);  %only some GPUs have enough data to make analysis worthwhile
        for i = 1:length(gpu_count)
            gpu_count(i) = sum(GPU_price{:,1} == i);
        end
        gpus_worth_using_binary = gpu_count>1000;
        gpus_worth_using = transpose(1:length(gpu_count));
        gpus_worth_using = gpus_worth_using(gpus_worth_using_binary);
        
        %filter the GPU price set to only use gpus with enough data points
        pass_rows = GPU_price{:,1}==transpose(gpus_worth_using);
        pass_rows1 = sum(pass_rows,2)==1;

    %region
        region_count = zeros(height(Region_factors),1);  %only some GPUs have enough data to make analysis worthwhile
        for i = 1:length(region_count)
            region_count(i) = sum(GPU_price{:,3} == i);
        end
        region_worth_using_binary = region_count>3000;
        region_worth_using = transpose(1:length(region_count));
        region_worth_using = region_worth_using(region_worth_using_binary);
        
        %filter the GPU price set to only use gpus with enough data points
        pass_rows = GPU_price{:,3}==transpose(region_worth_using);
        pass_rows2 = sum(pass_rows,2)==1;

    %seller
        seller_count = zeros(height(Merchant_factors),1);  %only some GPUs have enough data to make analysis worthwhile
        for i = 1:length(seller_count)
            seller_count(i) = sum(GPU_price{:,4} == i);
        end
        seller_worth_using_binary = seller_count>2000;
        seller_worth_using = transpose(1:length(seller_count));
        seller_worth_using = seller_worth_using(seller_worth_using_binary);
        
        %filter the GPU price set to only use gpus with enough data points
        pass_rows = GPU_price{:,4}==transpose(seller_worth_using);
        pass_rows3 = sum(pass_rows,2)==1;

%consolidate
pass_rows_all = and(and(pass_rows1,pass_rows2),pass_rows3);
GPU_price2 = GPU_price(pass_rows_all,1:5);




%% Plotting a subset of the data
gpu = 1936;
figure;

for i = 1:length(seller_worth_using)
    seller_single = seller_worth_using(i);
    filter_gpu = and((GPU_price2{:,1}==gpu),(GPU_price2{:,4}==seller_single));
    GPU_prices_plot = GPU_price2(filter_gpu,:);
        times_list = GPU_prices_plot{:,2};
    [time] = Time2DateTime(times_list);
    region = GPU_prices_plot{:,3};
    seller = GPU_prices_plot{:,4};
    price = GPU_prices_plot{:,5};

    scatter(time,price);

end












