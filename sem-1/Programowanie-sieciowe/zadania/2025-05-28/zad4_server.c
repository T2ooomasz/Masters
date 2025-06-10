#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/sctp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>

#define SERVER_STREAM_0 0
#define SERVER_STREAM_1 1

int main() {
  int sfd, on = 1, no, msg_flags = 0;
  socklen_t sl;
  char buf[1024];
  struct sctp_event_subscribe events;
  struct sctp_paddrparams heartbeat;
  struct sctp_rtoinfo rtoinfo;
  struct sctp_initmsg initmsg;
  struct sockaddr_in addr, raddr;

  sfd = socket(AF_INET, SOCK_SEQPACKET, IPPROTO_SCTP);
  setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(1234);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  bind(sfd, (struct sockaddr*)&addr, sizeof(addr));

  // Konfiguracja wielostrumieniowości
  memset(&initmsg, 0, sizeof(initmsg));
  initmsg.sinit_num_ostreams = 2;
  initmsg.sinit_max_instreams = 2;
  setsockopt(sfd, IPPROTO_SCTP, SCTP_INITMSG, &initmsg, sizeof(initmsg));

  // Heartbeat i RTO
  memset(&heartbeat, 0, sizeof(heartbeat));
  heartbeat.spp_flags = SPP_HB_ENABLE;
  heartbeat.spp_hbinterval = 2000;
  heartbeat.spp_pathmaxrxt = 1;
  setsockopt(sfd, SOL_SCTP, SCTP_PEER_ADDR_PARAMS, &heartbeat, sizeof(heartbeat));

  memset(&rtoinfo, 0, sizeof(rtoinfo));
  rtoinfo.srto_max = 2000;
  setsockopt(sfd, SOL_SCTP, SCTP_RTOINFO, &rtoinfo, sizeof(rtoinfo));

  memset(&events, 0, sizeof(events));
  events.sctp_data_io_event = 1;
  setsockopt(sfd, SOL_SCTP, SCTP_EVENTS, &events, sizeof(events));

  listen(sfd, 5);

  printf("Serwer nasłuchuje na porcie 1234...\n");

  while (1) {
    memset(buf, 0, sizeof(buf));
    memset(&raddr, 0, sizeof(raddr));
    sl = sizeof(raddr);

    no = sctp_recvmsg(sfd, buf, sizeof(buf), (struct sockaddr*)&raddr, &sl, NULL, &msg_flags);
    printf("Odebrano [%dB] od %s: %s\n", no, inet_ntoa(raddr.sin_addr), buf);

    // Odpowiedź naprzemiennie dwoma strumieniami
    static int stream_id = 0;
    sctp_sendmsg(sfd, buf, no, (struct sockaddr*)&raddr, sl, 0, 0, stream_id, 0, 0);
    stream_id = (stream_id + 1) % 2;

    sleep(1);
  }

  close(sfd);
  return 0;
}
