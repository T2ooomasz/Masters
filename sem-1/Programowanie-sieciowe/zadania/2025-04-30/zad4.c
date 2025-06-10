#include <arpa/inet.h>
#include <errno.h>
#include <linux/rtnetlink.h>
#include <net/if.h> // Required for if_nametoindex
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

// A request structure combining the Netlink message header, the route message header,
// and a buffer for route attributes.
struct route_request {
  struct nlmsghdr nlh;
  struct rtmsg rtm;
  char buf[4096];
};

/**
 * @brief Adds a routing attribute (rtattr) to a Netlink message.
 * @param req The route_request structure containing the message.
 *_ @param type The type of the attribute (e.g., RTA_DST, RTA_GATEWAY).
 * @param data A pointer to the attribute's data.
 * @param len The length of the attribute's data.
 */
void add_rtattr(struct route_request* req, int type, const void* data, int len) {
  struct rtattr* rta;
  // Point to the end of the current buffer
  rta = (struct rtattr*)(((char*)req) + NLMSG_ALIGN(req->nlh.nlmsg_len));
  // Set the attribute type and length
  rta->rta_type = type;
  rta->rta_len = RTA_LENGTH(len);
  // Copy the attribute data into the buffer
  memcpy(RTA_DATA(rta), data, len);
  // Update the total message length
  req->nlh.nlmsg_len = NLMSG_ALIGN(req->nlh.nlmsg_len) + rta->rta_len;
}

int main(int argc, char** argv) {
  if (argc < 5) {
    fprintf(stderr, "Usage: %s <destination_ip> <prefix_len> <gateway_ip> <interface>\n", argv[0]);
    fprintf(stderr, "Example: %s 192.168.50.0 24 10.0.2.2 enp0s8\n", argv[0]);
    return EXIT_FAILURE;
  }

  // --- 1. Parse Command-Line Arguments ---
  const char* dst_ip_str = argv[1];
  int prefix_len = atoi(argv[2]);
  const char* gw_ip_str = argv[3];
  const char* if_name = argv[4];

  // --- 2. Setup Netlink Socket ---
  int sfd = socket(PF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
  if (sfd < 0) {
    perror("socket(PF_NETLINK)");
    return EXIT_FAILURE;
  }

  // --- 3. Prepare the Netlink Message ---
  struct route_request req;
  memset(&req, 0, sizeof(req));

  // a) Fill the Netlink message header (nlmsghdr)
  req.nlh.nlmsg_len = NLMSG_LENGTH(sizeof(struct rtmsg));
  req.nlh.nlmsg_type = RTM_DELROUTE; // Message type: Delete Route
  req.nlh.nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK; // It's a request, and we want an acknowledgment
  req.nlh.nlmsg_seq = 1;
  req.nlh.nlmsg_pid = getpid();

  // b) Fill the Route message header (rtmsg)
  req.rtm.rtm_family = AF_INET;
  req.rtm.rtm_dst_len = prefix_len; // The prefix length of the destination
  req.rtm.rtm_src_len = 0;
  req.rtm.rtm_tos = 0;
  req.rtm.rtm_table = RT_TABLE_MAIN;
  req.rtm.rtm_protocol = RTPROT_STATIC;
  req.rtm.rtm_scope = RT_SCOPE_UNIVERSE;
  req.rtm.rtm_type = RTN_UNICAST;

  // c) Add routing attributes (rtattr) to the message
  struct in_addr dst_addr;
  if (inet_pton(AF_INET, dst_ip_str, &dst_addr) <= 0) {
    fprintf(stderr, "Invalid destination IP address: %s\n", dst_ip_str);
    close(sfd);
    return EXIT_FAILURE;
  }
  add_rtattr(&req, RTA_DST, &dst_addr, sizeof(dst_addr));

  struct in_addr gw_addr;
  if (inet_pton(AF_INET, gw_ip_str, &gw_addr) <= 0) {
    fprintf(stderr, "Invalid gateway IP address: %s\n", gw_ip_str);
    close(sfd);
    return EXIT_FAILURE;
  }
  add_rtattr(&req, RTA_GATEWAY, &gw_addr, sizeof(gw_addr));

  // Get the interface index dynamically from its name
  int if_index = if_nametoindex(if_name);
  if (if_index == 0) {
    fprintf(stderr, "Failed to find interface '%s': %s\n", if_name, strerror(errno));
    close(sfd);
    return EXIT_FAILURE;
  }
  add_rtattr(&req, RTA_OIF, &if_index, sizeof(if_index));

  // --- 4. Send the Message to the Kernel ---
  printf("Attempting to delete route: to %s/%d via %s dev %s...\n",
         dst_ip_str, prefix_len, gw_ip_str, if_name);

  if (send(sfd, &req, req.nlh.nlmsg_len, 0) < 0) {
    perror("send");
    close(sfd);
    return EXIT_FAILURE;
  }

  // --- 5. Receive Acknowledgment ---
  char recv_buf[4096];
  ssize_t len = recv(sfd, recv_buf, sizeof(recv_buf), 0);
  if (len < 0) {
    perror("recv");
    close(sfd);
    return EXIT_FAILURE;
  }
  
  struct nlmsghdr *nl_recv_hdr = (struct nlmsghdr *)recv_buf;
  if (nl_recv_hdr->nlmsg_type == NLMSG_ERROR) {
    struct nlmsgerr *err = (struct nlmsgerr *)NLMSG_DATA(nl_recv_hdr);
    if (err->error == 0) {
      printf("Route successfully deleted.\n");
    } else {
      fprintf(stderr, "Failed to delete route: %s\n", strerror(-err->error));
      close(sfd);
      return EXIT_FAILURE;
    }
  }

  // --- 6. Cleanup ---
  close(sfd);
  return EXIT_SUCCESS;
}