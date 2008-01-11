#!/usr/bin/perl -w
# Copyright (c) 2001-2008, AirWave Wireless, Inc.
# This material contains trade secrets and confidential information of AirWave
# Wireless, Inc.
# Any use, reproduction, disclosure or dissemination is strictly prohibited
# without the explicit written permission of AirWave Wireless, Inc.
# All rights reserved.

use strict;
use lib '/usr/local/airwave/lib/perl';

use Mercury::Preload::ForApache ();

# Some items for which it doesn't make sense to do anywhere but mod_perl:
use Apache2::Access ();
use Apache2::Connection ();
use Apache2::Reload ();
use Apache2::ServerUtil ();

#use Apache2::Status ();
#use Devel::Symdump ();
#use B::TerseSize ();
#
# Apache2::Status->menu_item(
#   'status_memory_usage' => "Memory Usage",
#   \&B::TerseSize::status_memory_usage,
# );

Mercury::AssertHandler->set_assert_handler();

# prime the pump (duplicated with M::Preload?)
Mercury::Handler::DispatcherMap->fetch_map;

# Pull in service list(s) (for AMPStatus handler)
Mercury::Daemon::SyncDProcessList->syncd_process_list();
Mercury::Daemon::ServiceWatcherList->service_list();

# Pull in some autloaded stuff
my $frozen = Storable::freeze({});
$frozen = Storable::nfreeze([]);
Storable::thaw($frozen);
eval { Storable::retrieve() };

1;
