use 5.010;
use ExtUtils::MakeMaker;

WriteMakefile( NAME           => 'Math::Geometry::RandomPlaneFiller',
               VERSION_FROM   => 'lib/Math/Geometry/RandomPlaneFiller.pm',
               PREREQ_PM      => { 'Math::Vector::Real' => '0.11',
                                   'Math::Vector::Random' => 0,
                                   'Math::Geometry::IntersectionArea' => 0, },
               ABSTRACT_FROM  => 'lib/Math/Geometry/RandomPlaneFiller.pm',
               AUTHOR         => 'Salvador Fandiño <sfandino@yahoo.com>',
);
