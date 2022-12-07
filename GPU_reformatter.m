clear all;close all;clc;

%% Header
%This script loads and converts csv files into a shape easier for me to
%analyze with R. No statistical analysis is performed.

%% Filepaths
filepath = "";
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
        GPU_factors = GPU_factors(gpus_worth_using,:);
        
        %filter the GPU price set to only use gpus with enough data points
        pass_rows = GPU_price{:,1}==transpose(gpus_worth_using);
        pass_rows1 = sum(pass_rows,2)==1;

    %region
        region_count = zeros(height(Region_factors),1);  %only some regions have enough data to make analysis worthwhile
        for i = 1:length(region_count)
            region_count(i) = sum(GPU_price{:,3} == i);
        end
        region_worth_using_binary = region_count>2000;
        region_worth_using = transpose(1:length(region_count));
        region_worth_using = region_worth_using(region_worth_using_binary);
        Region_factors = Region_factors(region_worth_using,:);

        %filter the GPU price set to only use gpus with enough data points
        pass_rows = GPU_price{:,3}==transpose(region_worth_using);
        pass_rows2 = sum(pass_rows,2)==1;

    %seller
        seller_count = zeros(height(Merchant_factors),1);  %only some sellers have enough data to make analysis worthwhile
        for i = 1:length(seller_count)
            seller_count(i) = sum(GPU_price{:,4} == i);
        end
        seller_worth_using_binary = seller_count>3000;
        seller_worth_using = transpose(1:length(seller_count));
        seller_worth_using = seller_worth_using(seller_worth_using_binary);
        Merchant_factors = Merchant_factors(seller_worth_using,:);
        
        %filter the GPU price set to only use gpus with enough data points
        pass_rows = GPU_price{:,4}==transpose(seller_worth_using);
        pass_rows3 = sum(pass_rows,2)==1;

%consolidate
pass_rows_all = and(and(pass_rows1,pass_rows2),pass_rows3);
GPU_price = GPU_price(pass_rows_all,1:5);


%% Identifying and eliminating all leverage points
    all_gpus = GPU_factors{:,1};
    filt_rows = zeros(size(GPU_price,1),1);
    rows_ind = 1:length(filt_rows);
    
    for i = 1:length(all_gpus)
        gpu = all_gpus(i);
    
        filter_gpu = GPU_price{:,1}==gpu;
        rows_use = rows_ind(filter_gpu');
        GPU_prices_plot = GPU_price(filter_gpu,:);
        price = GPU_prices_plot{:,5};
    
        n = sum(filter_gpu);
        p = 2;
        mean_price = mean(price);
        ss_price = sum((price-mean_price).^2);
        h = (1/n)+((price-mean_price).^2)./ss_price;
        h_thresh = 2*(p+1)/n;
        h_filt = h>h_thresh;
    
        filt_rows(rows_use) = h_filt;
    end
    GPU_price = GPU_price(~filt_rows,:);  %filtering out all leverage points

%% Filtering out all data to only happen when Crypto price is changing:
earliest_time = 20170000;
binary_rows_use = GPU_price{:,2}>earliest_time;
GPU_price = GPU_price(binary_rows_use,:);

%% Filtering out all data with GPU memory less than 4GB
lowest_memory= 4;
use_factors = GPU_factors{:,5}>=lowest_memory;
acceptable_gpus = GPU_factors{use_factors,1};
binary_usegpus = ismember(GPU_price{:,1}, acceptable_gpus);
GPU_price = GPU_price(binary_usegpus,:);


%% Finding mean price for each gpu, filtering to only use cards with sufficient mean price
    card_mean_prices = zeros(length(all_gpus),1);
for i = 1:length(all_gpus)
        gpu = all_gpus(i);
    
        filter_gpu = GPU_price{:,1}==gpu;
        rows_use = rows_ind(filter_gpu');
        GPU_prices_plot = GPU_price(filter_gpu,:);
        price = GPU_prices_plot{:,5};
        card_mean_prices(i) = mean(price);
end
thresh_gpu_price = 600;
cards_sufficient_price = card_mean_prices>thresh_gpu_price;
passed_cards = all_gpus(cards_sufficient_price);

    %filtering the gpu price dataset to only include information on these
    %cards
    acceptable_cards_bin = sum(GPU_price{:,1}==passed_cards',2)>0;
    GPU_price = GPU_price(acceptable_cards_bin,:);
    include_cards_bin = sum(GPU_factors{:,1}==passed_cards',2)>0;
    GPU_factors = GPU_factors(include_cards_bin,:);


%% Filtering out all merchants/sellers for specific cards
        %If a merchant doesn't have enough information (some threshold of sold
        %cards) on a specific graphics card, don't report that data through
        card_seller_threshold = 50;
        for i = 1:size(GPU_factors,1)
            for j = 1:size(Merchant_factors,1)
                row_counter = (1:size(GPU_price))';
                bin_cardseller = and((GPU_price{:,1} == GPU_factors{i,1}),(GPU_price{:,4} == Merchant_factors{j,1}));
                if (sum(bin_cardseller)<card_seller_threshold)&&(sum(bin_cardseller)>0) %delete rows
                    GPU_price(row_counter(bin_cardseller),:) = [];
                end
            end
        end

%% Filtering out gpus that are only sold from one website
for i = 1:size(GPU_factors,1)
    gpu_id = GPU_factors{i,1};
        unique_merchants = and((GPU_price{:,1} == GPU_factors{i,1}),(GPU_price{:,4} == Merchant_factors{j,1}));
        if (sum(bin_cardseller)<card_seller_threshold)&&(sum(bin_cardseller)>0) %delete rows
            GPU_price(row_counter(bin_cardseller),:) = [];
        end
end

%% Filtering out cryptocurrencies
crypto_binary = Crypto_factors{:,4}==1;
Crypto_factors = Crypto_factors(crypto_binary,:);
crypto_labels = Crypto_factors{:,1};
use_cryptos = sum(Crypto_price{:,1}==crypto_labels',2)>0;
Crypto_price = Crypto_price(use_cryptos,:);

%% Plotting a subset of the data

%plotting all merchants/regions together
% figure(2);
% cards = GPU_factors{:,1};
% for i = 1:length(cards)
% filter_gpu = GPU_price{:,1}==cards(i);
% GPU_prices_plot = GPU_price(filter_gpu,:);
% time = Time2DateTime(GPU_prices_plot{:,2});
% region = GPU_prices_plot{:,3};
% seller = GPU_prices_plot{:,4};
% price = GPU_prices_plot{:,5};
% 
% card_name = string(GPU_factors{i,3});
% 
% scatter(time,price);
% ylabel('GPU Price');
% xlabel('Sell Date')
% title(['Scatterplot of ',card_name,' price over time, all sellers'])
% hold off;
% end


Crypto_factors_filename_out     = "DIM_CRYPTO_DATA_filt.csv";
GPU_factors_filename_out        = "DIM_GPU_PROD_filt.csv";
Merchant_factors_filename_out   = "DIM_MERCHANT_filt.csv";
Region_factors_filename_out     = "DIM_REGION_filt.csv";
Time_factors_filename_out       = "DIM_TIME_filt.csv";
GPU_price_filename_out          = "FACT_GPU_PRICE_filt.csv";
Crypto_price_filename_out       = "FACT_CRYPTO_RATE_filt.csv";

writetable(Crypto_factors,Crypto_factors_filename_out)
writetable(GPU_factors,GPU_factors_filename_out)
writetable(Merchant_factors,Merchant_factors_filename_out)
writetable(Region_factors,Region_factors_filename_out)
writetable(Time_factors,Time_factors_filename_out)
writetable(GPU_price,GPU_price_filename_out)
writetable(Crypto_price,Crypto_price_filename_out)















