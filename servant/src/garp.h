/*
The contents of this file are subject to the Erlang Public License,
Version 1.0, (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.eddieware.org/EPL

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.

The Original Code is Eddie-0.83b1.

The Initial Developer of the Original Code is Ericsson Telecom
AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
Telecom AB. All Rights Reserved.

Contributor(s): ______________________________________.
*/


struct eth_arphdr {
  u_int16_t ar_hrd;       /* Format of hardware address. */
  u_int16_t ar_pro;       /* Format of protocol address. */
  u_int8_t ar_hln;            /* Length of hardware address. */
  u_int8_t ar_pln;            /* Length of protocol address. */
  u_int16_t ar_op;        /* ARP opcode (command). */
  u_int8_t ar_sha[ETHER_ADDR_LEN];  /* Sender hardware address. */
  u_int8_t ar_sip[4];         /* Sender IP address. */
  u_int8_t ar_tha[ETHER_ADDR_LEN];  /* Target hardware address. */
  u_int8_t ar_tip[4];         /* Target IP address. */
};
