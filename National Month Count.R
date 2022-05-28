##############National Data Set##################3

#Looking at indicators by month
library(tidyverse)
library(writexl)

#Import dataset
national <- read_excel("National1.xlsx")

colnames(national)
class(National1$Report_Date)

##Registration Count###

#Monthly Count
Sum_NHSApp_RegistrationsCount_count <- national %>%
  dplyr::select(Report_Date, Sum_NHSApp_RegistrationsCount) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  Sum_NHSApp_RegistrationsCount) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Sum_NHSApp_RegistrationsCount)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(Sum_NHSApp_RegistrationsCount_count, 'Sum_NHSApp_RegistrationsCount_count.xlsx')


#Weekly Count
Sum_NHSApp_RegistrationsCount_weekly <- national %>%
  dplyr::select(Report_Date, Sum_NHSApp_RegistrationsCount) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "week")) %>%
  drop_na() %>%
  dplyr::select(month,  Sum_NHSApp_RegistrationsCount) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Sum_NHSApp_RegistrationsCount)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(Sum_NHSApp_RegistrationsCount_weekly, 'Sum_NHSApp_RegistrationsCount_weekly.xlsx')


##Login Sessions Count###

Sum_Usage_LoginSessions_Login_Sessions_count <- national %>%
  dplyr::select(Report_Date, Sum_Usage_LoginSessions_Login_Sessions) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  Sum_Usage_LoginSessions_Login_Sessions) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Sum_Usage_LoginSessions_Login_Sessions)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(Sum_Usage_LoginSessions_Login_Sessions_count, 'Sum_Usage_LoginSessions_Login_Sessions_count.xlsx')


##Appts Booked Count###

Sum_Usage_Appointments_Appointments_booked_count <- national %>%
  dplyr::select(Report_Date, Sum_Usage_Appointments_Appointments_booked) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  Sum_Usage_Appointments_Appointments_booked) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Sum_Usage_Appointments_Appointments_booked)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(Sum_Usage_Appointments_Appointments_booked_count, 'Sum_Usage_Appointments_Appointments_booked_count.xlsx')

##Appts Cancelled Count###

Sum_Usage_CancelledAppointments_Cancellation_Count_count <- national %>%
  dplyr::select(Report_Date, Sum_Usage_CancelledAppointments_Cancellation_Count) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  Sum_Usage_CancelledAppointments_Cancellation_Count) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Sum_Usage_CancelledAppointments_Cancellation_Count)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(Sum_Usage_CancelledAppointments_Cancellation_Count_count, 'Sum_Usage_CancelledAppointments_Cancellation_Count_count.xlsx')


##Medical Records Viewed Count##

Sum_Usage_MedicalRecords_Medical_record_views_count <- national %>%
  dplyr::select(Report_Date, Sum_Usage_MedicalRecords_Medical_record_views) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  Sum_Usage_MedicalRecords_Medical_record_views) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Sum_Usage_MedicalRecords_Medical_record_views)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(Sum_Usage_MedicalRecords_Medical_record_views_count, 'Sum_Usage_MedicalRecords_Medical_record_views_count.xlsx')


##Organ Donation Registrations##

Sum_Usage_OrganDonation_RegistrationsODR_count <- national %>%
  dplyr::select(Report_Date, Sum_Usage_OrganDonation_RegistrationsODR) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  Sum_Usage_OrganDonation_RegistrationsODR) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Sum_Usage_OrganDonation_RegistrationsODR)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(Sum_Usage_OrganDonation_RegistrationsODR_count, 'Sum_Usage_OrganDonation_RegistrationsODR_count.xlsx')


#Prescriptions Ordered Count##

Sum_Usage_Prescriptions_Prescriptions_Ordered_count <- national %>%
  dplyr::select(Report_Date, Sum_Usage_Prescriptions_Prescriptions_Ordered) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  Sum_Usage_Prescriptions_Prescriptions_Ordered) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Sum_Usage_Prescriptions_Prescriptions_Ordered)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()


write_xlsx(Sum_Usage_Prescriptions_Prescriptions_Ordered_count , 'Sum_Usage_Prescriptions_Prescriptions_Ordered_count.xlsx')

##iOS_installs##
iOS_installs_count <- national %>%
  dplyr::select(Report_Date, iOS_installs) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  iOS_installs) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(iOS_installs)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(iOS_installs_count , 'iOS_installs_countt.xlsx')


##Android installs##

Android_installs_count <- national %>%
  dplyr::select(Report_Date, Android_installs) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  Android_installs) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Android_installs)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(Android_installs_count , 'Android_installs_count.xlsx')



##Usage_111_weekly_Link_Click##

Usage_111_weekly_Link_Click_count <- national %>%
  dplyr::select(Report_Date, Usage_111_weekly_Link_Click) %>%
  dplyr::mutate(month = lubridate::floor_date(Report_Date, "month")) %>%
  drop_na() %>%
  dplyr::select(month,  Usage_111_weekly_Link_Click) %>%
  dplyr::group_by(month) %>%
  mutate(Total = sum(Usage_111_weekly_Link_Click)) %>%
  dplyr::select(month, Total) %>%
  dplyr::distinct()

write_xlsx(Usage_111_weekly_Link_Click_count , 'Usage_111_weekly_Link_Click_count.xlsx')
