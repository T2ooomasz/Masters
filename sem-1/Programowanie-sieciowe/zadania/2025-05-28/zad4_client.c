#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/sctp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>

#define CLIENT_STREAM_0 0
#define CLIENT_STREAM_1 1

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("Użycie: %s <IP serwera>\n", argv[0]);
    exit(1);
  }

  int sfd, no, msg_flags = 0;
  socklen_t sl;
  char buf[1024];
  struct sctp_event_subscribe events;
  struct sctp_initmsg initmsg;
  struct sctp_paddrparams heartbeat;
  struct sctp_rtoinfo rtoinfo;
  struct sockaddr_in saddr, raddr;

  sfd = socket(AF_INET, SOCK_STREAM, IPPROTO_SCTP);

  memset(&saddr, 0, sizeof(saddr));
  saddr.sin_family = AF_INET;
  saddr.sin_port = htons(1234);
  inet_pton(AF_INET, argv[1], &saddr.sin_addr);

  // Konfiguracja wielu strumieni
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

  connect(sfd, (struct sockaddr*)&saddr, sizeof(saddr));

  printf("Połączono z %s\n", argv[1]);

  while (1) {
    static int stream_id = 0;
    sctp_sendmsg(sfd, "Echo message", 13, NULL, 0, 0, 0, stream_id, 0, 0);

    memset(&raddr, 0, sizeof(raddr));
    memset(buf, 0, sizeof(buf));
    sl = sizeof(raddr);
    no = sctp_recvmsg(sfd, buf, sizeof(buf), (struct sockaddr*)&raddr, &sl, NULL, &msg_flags);
    printf("Odebrano [%dB] od %s: %s\n", no, inet_ntoa(raddr.sin_addr), buf);

    stream_id = (stream_id + 1) % 2;
    sleep(1);
  }

  close(sfd);
  return 0;
}
