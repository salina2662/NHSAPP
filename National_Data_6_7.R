# National Data 6-14-21
library(tidyverse)
library(writexl)
library(reader)

#Set wd
setwd("~/NHS APP/National")

#Original 
National<-read_csv("National.csv", col_types = cols(Report_Date = col_date(format = "%m/%d/%Y"), 
                                                        +     Sum_NHSApp_RegistrationsCount = col_number(), 
                                                        +     Sum_Usage_LoginSessions_Login_Sessions = col_number(), 
                                                        +     Sum_Usage_Appointments_Appointments_booked = col_number(), 
                                                        +     Sum_Usage_CancelledAppointments_Cancellation_Count = col_number(), 
                                                        +     Sum_Usage_MedicalRecords_Medical_record_views = col_number(), 
                                                        +     Sum_Usage_OrganDonationRegUpdates_SuccessfulUpdates = col_number(), 
                                                        +     Sum_Usage_OrganDonationRegWithdrawals_SuccessfulUpdates = col_number(), 
                                                        +     Sum_Usage_OrganDonation_RegistrationsODR = col_number(), 
                                                        +     Sum_Usage_Prescriptions_Prescriptions_Ordered = col_number(), 
                                                        +     Sum_Usage_Appointments_weekly_Unique_Visitors = col_number(), 
                                                        +     Sum_Usage_medicalrecord_weekly_Unique_Visitors = col_number(), 
                                                        +     Sum_Usage_Prescriptions_weekly_Unique_Visitors = col_number()))

#May 9 2021

National_5_9<-read_csv("National_5_9.csv", 
                      +     col_types = cols(Report_Date = col_date(format = "%m/%d/%Y"), 
                                             +         Sum_NHSApp_RegistrationsCount = col_number(), 
                                             +         Sum_Usage_LoginSessions_Login_Sessions = col_number(), 
                                             +         Sum_Usage_Appointments_Appointments_booked = col_number(), 
                                             +         Sum_Usage_CancelledAppointments_Cancellation_Count = col_number(), 
                                             +         Sum_Usage_MedicalRecords_Medical_record_views = col_number(), 
                                             +         Sum_Usage_OrganDonationRegUpdates_SuccessfulUpdates = col_number(), 
                                             +         Sum_Usage_OrganDonationRegWithdrawals_SuccessfulUpdates = col_number(), 
                                             +         Sum_Usage_OrganDonation_RegistrationsODR = col_number(), 
                                             +         Sum_Usage_Prescriptions_Prescriptions_Ordered = col_number(), 
                                             +         Sum_Usage_Appointments_weekly_Unique_Visitors = col_number(), 
                                             +         Sum_Usage_medicalrecord_weekly_Unique_Visitors = col_number(), 
                                             +         Sum_Usage_Prescriptions_weekly_Unique_Visitors = col_number()))

#June 7 2021

National_6_7<-read_csv("National_6_7.csv", 
                      +     col_types = cols(Report_Date = col_date(format = "%m/%d/%Y"), 
                                             +         Sum_NHSApp_RegistrationsCount = col_number(), 
                                             +         Sum_Usage_LoginSessions_Login_Sessions = col_number(), 
                                             +         Sum_Usage_Appointments_Appointments_booked = col_number(), 
                                             +         Sum_Usage_CancelledAppointments_Cancellation_Count = col_number(), 
                                             +         Sum_Usage_MedicalRecords_Medical_record_views = col_number(), 
                                             +         Sum_Usage_OrganDonationRegUpdates_SuccessfulUpdates = col_number(), 
                                             +         Sum_Usage_OrganDonationRegWithdrawals_SuccessfulUpdates = col_number(), 
                                             +         Sum_Usage_OrganDonation_RegistrationsODR = col_number(), 
                                             +         Sum_Usage_Prescriptions_Prescriptions_Ordered = col_number(), 
                                             +         Sum_Usage_Appointments_weekly_Unique_Visitors = col_number(), 
                                             +         Sum_Usage_medicalrecord_weekly_Unique_Visitors = col_number(), 
                                             +         Sum_Usage_Prescriptions_weekly_Unique_Visitors = col_number()))

#Merge org & 5_9
national_org_5_9<- rbind(National , National_5_9)

#Merge 5_9 & 6_7
national_new_6_21<- rbind(national_org_5_9 , National_6_7)
write_csv(national_new_6_21, "national_new_6_21.csv")


colnames(national_new_6_21)

national_new_6_21_month <- national_new_6_21 %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  dplyr::select(month, Sum_NHSApp_RegistrationsCount,Sum_Usage_LoginSessions_Login_Sessions, 
                Sum_Usage_Appointments_Appointments_booked, 
                Sum_Usage_MedicalRecords_Medical_record_views,
                Sum_Usage_OrganDonation_RegistrationsODR,
                Sum_Usage_Prescriptions_Prescriptions_Ordered ) %>%
  tidyr::pivot_longer(cols = c(2:7), names_to = "type", values_to = "test" ) %>%
  tidyr::drop_na() %>%
  dplyr::group_by(month, type) %>%
  summarize(count=sum(test)) 


write_csv(national_new_6_21_month, "national_new_6_21_month.csv")

