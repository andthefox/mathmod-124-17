# �������� ������ ������������� �������� ��������� ������� ������� ����� ���� �� �������� ������ 2013 ���� 
# �� ������ ��������� ������� ������������ ���������
# ��� ������ ������ ����� ����������� ���������� DOY - ���� ���� (1 ������ - DOY = 1)

#���������� tidyverse
library(tidyverse)
#������ ������ �� �����, ���������� ....., �������� ...., 
data = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
data = data[-1,]
#������� �������� �������
data = data[, c(-1, -3, -9, -12, -15, -18, -21, -30, -35, -63 , -70, -88:-99) ]
#����������� ��������� �������� � ���������
data = data %>% mutate_if(is.character, factor)
#�������� ������������� ����� �������
names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>%
  str_replace_all("[?]","_quest_") %>%  
  str_replace_all("[*]","_star_") %>%  
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(data)
