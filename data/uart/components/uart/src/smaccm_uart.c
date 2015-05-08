#include <uart.h>

bool Input_send_write_Data_Types__ivory_string_UartPacket_impl(const Data_Types__ivory_string_UartPacket_impl * input) {
  int r = uart_write(input->ivory_string_UartPacket_len, input->ivory_string_UartPacket_data);
  if (r < 0) {
    printf("Error from uart_write, return code: %d\n", r);
  }
  return (r >= 0);
}
