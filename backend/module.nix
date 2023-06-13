{ package, frontend, system }:
{ config, lib, pkgs, ... }:

let
  cfg = config.services.sdc-map-backend;

  serveDir = frontend.outputs.defaultPackage.${system};
in
{
  options = {
    services.sdc-map-backend = {
      enable = lib.mkEnableOption ''
        sdc-map-backend: The backend for the sdc map project.
      '';

      port = lib.mkOption {
        type = lib.types.int;
        default = 3000;
        description = ''
          The port that sdc-map-backend binds to listen for connections.
        '';
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = package;
        description = ''
          The sdc-map-backend package to use with the service.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.sdc-map-backend = {
      description = "sdc-map-backend daemon user";
      isSystemUser = true;
      group = "sdc-map-backend";
    };

    users.groups.sdc-map-backend = { };

    systemd.services.sdc-map-backend = {
      description = "sdc-map-backend";

      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        User = "sdc-map-backend";
        Group = "sdc-map-backend";
        # Restart = "always";
        ExecStart = "${lib.getBin cfg.package}/bin/sdc-map-backend \"${serveDir}\" ${toString cfg.port} \${STATE_DIRECTORY}/db.sqlite";
        StateDirectory = "sdc-map-backend";
        StateDirectoryMode = "0750";

        # Hardening
        # CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
        # DeviceAllow = [ ];
        # DevicePolicy = "strict";
        # IPAddressAllow = "localhost";
        # LockPersonality = true;
        # MemoryDenyWriteExecute = true;
        # NoNewPrivileges = true;
        # PrivateDevices = true;
        # PrivateTmp = true;
        # PrivateUsers = true;
        # ProtectClock = true;
        # ProtectControlGroups = true;
        # ProtectHome = true;
        # ProtectHostname = true;
        # ProtectKernelLogs = true;
        # ProtectKernelModules = true;
        # ProtectKernelTunables = true;
        # ProtectSystem = "strict";
        # ReadOnlyPaths = [ serveDir ];
        # ReadWritePaths = [ cfg.dataDir ];
        # RemoveIPC = true;
        # RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];
        # RestrictNamespaces = true;
        # RestrictRealtime = true;
        # RestrictSUIDSGID = true;
        # SystemCallArchitectures = "native";
        # SystemCallFilter = [ "@system-service" "~@privileged" "~@resources" "@pkey" ];
        # UMask = "0027";
      };
    };
  };
}
